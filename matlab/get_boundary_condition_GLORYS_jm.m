%created by G.Xu
%modified by J. Mette, 20/04/2024

%==========================================================================
% This script creates boundary conditions from NEMO dataset.
%==========================================================================

clc;
clear;
close all;

%--------------------------------------------------------------------------
%  Initialization
%--------------------------------------------------------------------------
%addpath('../roms_matlab/grid');
%addpath('../roms_matlab/utility');
%addpath('../roms_matlab/boundary');
%addpath('../roms_matlab/netcdf');
%addpath('../roms_matlab/mexcdf/mexnc');

app_dir = '/home/jonathan/Dokumente/model/roms_project/aurora/matlab';
GRD_file = 'Grid_Aurora_M64L64.nc';
GRDname = fullfile(app_dir,GRD_file);
NEMO_dir = 'Dokumente/model/inputs/boundary_conditions';
NEMO_name = 'cmems_mod_glo_phy_myint_0.083deg_P1M-m_1719414538489.nc';
NEMOfile = fullfile(NEMO_dir,NEMO_name);

% Get NEMO time
time_nemo=ncread(NEMOfile,'time');
date_nemo=time_nemo/24+datenum(1950,1,1,0,0,0); % convert to Matlab datenum (the time axis in NEMO data starts from Jan 1, 1950)
nemo_ntime = 14;  %number of timesteps in nemo file

% set start and end dates of the simulation
date_ref = date_nemo(1);
date_start = date_nemo(1);
date_end = date_nemo(end);
% This block works with non-interim files
date_ref = datenum(2022,07,26,00,00,00);    
date_start = date_ref;
date_end = datenum(2023,07,01,00,00,00);
date_nemo=linspace(date_start,date_end,nemo_ntime);

% set output file directory and name
%BRY_dir = app_dir;
BRY_file = sprintf('%s_bry_nemo_%s_%s.nc',GRD_file(1:end-3),datestr(date_start,'yyyymmddTHH'),datestr(date_end,'yyyymmddTHH'));
BRYname = fullfile(app_dir,BRY_file);

CREATE = 1;                      % logical switch to create NetCDF
WRITE  = 1;                      % logical switch to write out data
report = 1;                      % report vertical grid information

% Get number of grid points
Sinp.N = 20;                    % number of vertical levels at RHO-points
Sinp.Vtransform = 2;
Sinp.Vstretching = 4;
Sinp.theta_s=0;
Sinp.theta_b=1;
Sinp.Tcline = 20;
Sinp.hc = 20;
G = get_roms_grid(GRDname,Sinp);

%--------------------------------------------------------------------------
%  Set application parameters in structure array, S.
%--------------------------------------------------------------------------
S = G;
S.ncname      = BRYname;    % output NetCDF file
S.NT = 2;



%  Set switches for boundary segments to process.

OBC.west  = true;           % process western  boundary segment
OBC.east  = true;           % process eastern  boundary segment
OBC.south = true;           % process southern boundary segment
OBC.north = true;          % process northern boundary segment

S.boundary(1) = OBC.west;
S.boundary(2) = OBC.east;
S.boundary(3) = OBC.south;
S.boundary(4) = OBC.north;

%--------------------------------------------------------------------------
%  Set variables to process.
%--------------------------------------------------------------------------

%  Grid variables.

VarGrd = {'spherical',                                                ...
    'Vtransform', 'Vstretching',                                ...
    'theta_s', 'theta_b', 'Tcline', 'hc',                       ...
    's_rho', 'Cs_r', 's_w', 'Cs_w'};

if (S.spherical)
    if (OBC.west)
        VarGrd = [VarGrd, 'lon_rho_west',  'lat_rho_west',                ...
            'lon_u_west',    'lat_u_west',                  ...
            'lon_v_west',    'lat_v_west'];
    end
    if (OBC.east)
        VarGrd = [VarGrd, 'lon_rho_east',  'lat_rho_east',                ...
            'lon_u_east',    'lat_u_east',                  ...
            'lon_v_east',    'lat_v_east'];
    end
    if (OBC.south)
        VarGrd = [VarGrd, 'lon_rho_south', 'lat_rho_south',               ...
            'lon_u_south',   'lat_u_south',                 ...
            'lon_v_south',   'lat_v_south'];
    end
    if (OBC.north)
        VarGrd = [VarGrd, 'lon_rho_north', 'lat_rho_north',               ...
            'lon_u_north',   'lat_u_north',                 ...
            'lon_v_north',   'lat_v_north'];
    end
else
    if (OBC.west)
        VarGrd = [VarGrd, 'x_rho_west',  'y_rho_west',                    ...
            'x_u_west',    'y_u_west',                      ...
            'x_v_west',    'y_v_west'];
    end
    if (OBC.east)
        VarGrd = [VarGrd, 'x_rho_east',  'y_rho_east',                    ...
            'x_u_east',    'y_u_east',                      ...
            'x_v_east',    'y_v_east'];
    end
    if (OBC.south)
        VarGrd = [VarGrd, 'x_rho_south', 'y_rho_south',                   ...
            'x_u_south',   'y_u_south',                     ...
            'x_v_south',   'y_v_south'];
    end
    if (OBC.north)
        VarGrd = [VarGrd, 'x_rho_north', 'y_rho_north',                   ...
            'x_u_north',   'y_u_north',                     ...
            'x_v_north',   'y_v_north'];

    end
end

%  ROMS state variables to process.  In 3D applications, the 2D momentum
%  components (ubar,vbar) are compouted by vertically integrating
%  3D momentum component. Therefore, interpolation of (ubar,vbar) is
%  not carried out for efficiency.

VarBry  = {'zeta', 'u', 'v', 'temp', 'salt'};
VarList = [VarBry, 'ubar', 'vbar'];

%  Set intepolation parameters.

method = 'linear';             % linear interpolation
offset = 0;                    % number of extra points for sampling
RemoveNaN = true;              % remove NaN with nearest-neighbor
Rvector = true;                % interpolate vectors to RHO-points



%---------------------------------------------------------------------------
%  Create boundary condition Netcdf file.
%---------------------------------------------------------------------------

if (CREATE)

    [status]=c_boundary(S);

    %  Set attributes for "bry_time".
    
    avalue='seconds since 2022-07-26 00:00:00';
    %avalue=sprintf('seconds since %s',datestr(date_ref,'yyyy-mm-dd HH:MM:SS'));
    [status]=nc_attadd(BRYname,'units',avalue,'bry_time');

    avalue='gregorian';
    [status]=nc_attadd(BRYname,'calendar',avalue,'bry_time');

    %  Set global attribute.

    avalue='Axial Circulation';
    [status]=nc_attadd(BRYname,'title',avalue);

    avalue='ROMS Axial application';
    [status]=nc_attadd(BRYname,'source',avalue);

    [status]=nc_attadd(BRYname,'grd_file',GRDname);

    %  Write out grid data.

    for var = VarGrd
        field = char(var);
        [err.(field)] = nc_write(BRYname, field, S.(field));
    end

end

%---------------------------------------------------------------------------
%  Interpolate boundary conditions from NEMO data to application grid.
%---------------------------------------------------------------------------

disp(' ');
disp(['**************************************************************']);
disp(['** Interpolating boundaries conditions from NEMO to ROMS grid **']);
disp(['**************************************************************']);



%  Get NEMO grid.
ncout = ncinfo(NEMOfile);
Pz = -ncread(NEMOfile,'depth');
Plat = ncread(NEMOfile,'latitude');
Plon = ncread(NEMOfile,'longitude');
Plon = double(Plon);
Plat = double(Plat);
Pz = double(Pz);
[Pllon,Pllat] = meshgrid(Plon,Plat);

% calculate time indices for the period of interest
tind = find(date_nemo>=date_start&date_nemo<=date_end);


%--------------------------------------------------------------------------
% interpolate and write boundary conditions
%--------------------------------------------------------------------------

% coordinates of ROMS grid points on the boundaries
Tlon = S.lon_rho;
Tlat = S.lat_rho;
zr = S.z_r;
angle=repmat(S.angle,[1,1,S.N]);
Hz = S.Hz;
Tlon_br = cell(1,4);
Tlon_bu = cell(1,4);
Tlat_br = cell(1,4);
Tlat_bu = cell(1,4);
z_br = cell(1,4);
z_bu = cell(1,4);
angle_bu = cell(1,4);
Hz_bu = cell(1,4);
for ib = 1:4
    switch ib
        case 1 % western boundary
            Tlon_br{ib} = Tlon(1,:);
            Tlat_br{ib} = Tlat(1,:);
            Tlon_bu{ib} = Tlon(1:2,:);
            Tlat_bu{ib} = Tlat(1:2,:);
            z_br{ib} = zr(1,:,:);
            z_bu{ib} = zr(1:2,:,:);
            angle_bu{ib} = angle(1:2,:,:);
            Hz_bu{ib} = Hz(1:2,:,:);
        case 2 % eastern boundary
            Tlon_br{ib} = Tlon(end,:);
            Tlat_br{ib} = Tlat(end,:);
            Tlon_bu{ib} = Tlon(end-1:end,:);
            Tlat_bu{ib} = Tlat(end-1:end,:);
            z_br{ib} = zr(end,:,:);
            z_bu{ib} = zr(end-1:end,:,:);
            angle_bu{ib} = angle(end-1:end,:,:);
            Hz_bu{ib} = Hz(end-1:end,:,:);
        case 3 % southern boundary
            Tlon_br{ib} = Tlon(:,1);
            Tlat_br{ib} = Tlat(:,1);
            Tlon_bu{ib} = Tlon(:,1:2);
            Tlat_bu{ib} = Tlat(:,1:2);
            z_br{ib} = zr(:,1,:);
            z_bu{ib} = zr(:,1:2,:);
            angle_bu{ib} = angle(:,1:2,:);
            Hz_bu{ib} = Hz(:,1:2,:);
        case 4 % northern boundary
            Tlon_br{ib} = Tlon(:,end);
            Tlat_br{ib} = Tlat(:,end);
            Tlon_bu{ib} = Tlon(:,end-1:end);
            Tlat_bu{ib} = Tlat(:,end-1:end);
            z_br{ib} = zr(:,end,:);
            z_bu{ib} = zr(:,end-1:end,:);
            angle_bu{ib} = angle(:,end-1:end,:);
            Hz_bu{ib} = Hz(:,end-1:end,:);
    end
end

% index of the lowest level in NEMO to be used
zind = find(Pz<min(S.z_r(:)),1);

BryRec = 0;
for it = 1:length(tind)
    tind1 = tind(it);
    S.bry_time = (date_nemo(tind1)-date_ref)*24*3600; % boundary condition time (seconds since the start)
    Temp = ncread(NEMOfile,'thetao',[1 1 1 tind1],[inf,inf,zind,1]);
    Salt = ncread(NEMOfile,'so',[1 1 1 tind1],[inf,inf,zind,1]);
    Btemp = ncread(NEMOfile,'bottomT',[1 1 tind1],[inf,inf,1]);
    Zeta = ncread(NEMOfile,'zos',[1 1 tind1],[inf,inf,1]);
    Uvel = ncread(NEMOfile,'uo',[1 1 1 tind1],[inf,inf,zind,1]);
    Vvel = ncread(NEMOfile,'vo',[1 1 1 tind1],[inf,inf,zind,1]);
    Zeta = double(Zeta);
    Temp = double(Temp);
    Btemp = double(Btemp);
    Salt = double(Salt);
    Uvel = double(Uvel);
    Vvel = double(Vvel);
    Zeta_p = permute(Zeta,[2,1]);
    Btemp_p = permute(Btemp,[2,1]);
    Temp_p = permute(Temp,[2,1,3]);
    Salt_p = permute(Salt,[2,1,3]);
    Uvel_p = permute(Uvel,[2,1,3]);
    Vvel_p = permute(Vvel,[2,1,3]);
    clear Zeta Temp Salt Uvel Vvel


    % loop over four boundaries
    FSb = cell(1,4);
    Tb = cell(1,4);
    Sb = cell(1,4);
    Ub = cell(1,4);
    Vb = cell(1,4);
    Ubarb = cell(1,4);
    Vbarb = cell(1,4);
    dummy = ones(size(Pllon));
    F_2D = scatteredInterpolant(Pllon(:),Pllat(:),dummy(:));
    for ib = 1:4
        lonr = Tlon_br{ib};
        lonu = Tlon_bu{ib};
        latr = Tlat_br{ib};
        latu = Tlat_bu{ib};

        % interpolate free-surface boundary conditions

        % first round of interpolation
        F_2D.Values = Zeta_p(:);
        zeta = F_2D(lonr,latr);
        F_2D.Values = Btemp_p(:);
        btemp = F_2D(lonr,latr);

        % second round to replace NaNs with the nearest valid values
        ind = find(isnan(zeta));
        if ~isempty(ind)
            ii = find(~isnan(Zeta_p(:)));
            F_2D2 = scatteredInterpolant(Pllon(ii),Pllat(ii),Zeta_p(ii),'nearest');
            zeta(ind) = F_2D2(lonr(ind),latr(ind));
            F_2D2 = scatteredInterpolant(Pllon(ii),Pllat(ii),Btemp_p(ii),'nearest');
            btemp(ind) = F_2D2(lonr(ind),latr(ind));
        end
        FSb{ib} = zeta;

        % interpolate 3D variables
        Tempb = zeros(size(lonr,1),size(lonr,2),zind);
        Saltb = zeros(size(lonr,1),size(lonr,2),zind);
        Uvelb = zeros(size(lonu,1),size(lonu,2),zind);
        Vvelb = zeros(size(lonu,1),size(lonu,2),zind);
        for e = 1:zind

            % first round of interpolation
            Temp1 = Temp_p(:,:,e);
            F_2D.Values = Temp1(:);
            Temp_int1 = F_2D(lonr,latr);
            Salt1 = Salt_p(:,:,e);
            F_2D.Values = Salt1(:);
            Salt_int1 = F_2D(lonr,latr);
            Uvel1 = Uvel_p(:,:,e);
            F_2D.Values = Uvel1(:);
            U_int1 = F_2D(lonu,latu);
            Vvel1 = Vvel_p(:,:,e);
            F_2D.Values = Vvel1(:);
            V_int1 = F_2D(lonu,latu);

            % second round to replace NaNs with the nearest valid values
                        % ind = find(isnan(Temp_int1));
                        % if ~isempty(ind)
                        %     ii = find(~isnan(Temp1(:)));
                        %     F_2D2 = scatteredInterpolant(Pllon(ii),Pllat(ii),Temp1(ii),'nearest');
                        %     Temp_int1(ind) = F_2D2(lonr(ind),latr(ind));
                        %     F_2D2.Values = Salt1(ii);
                        %     Salt_int1(ind) = F_2D2(lonr(ind),latr(ind));
                        % end
            % second round end
            Tempb(:,:,e) = Temp_int1;
            Saltb(:,:,e) = Salt_int1;

            % second round to replace NaNs with the nearest valid values
                        % ind = find(isnan(U_int1));
                        % if ~isempty(ind)
                        %     ii = find(~isnan(Uvel1(:)));
                        %     F_2D2 = scatteredInterpolant(Pllon(ii),Pllat(ii),Uvel1(ii),'nearest');
                        %     U_int1(ind) = F_2D2(lonu(ind),latu(ind));
                        %     F_2D2.Values = Vvel1(ii);
                        %     V_int1(ind) = F_2D2(lonu(ind),latu(ind));
                        % end
            % second round end
            Uvelb(:,:,e) = U_int1;
            Vvelb(:,:,e) = V_int1;
        end

        % interpolate temperature/salinity in the vertical direction
        temp = zeros(size(lonr,1),size(lonr,2),size(S.z_r,3));
        salt = zeros(size(lonr,1),size(lonr,2),size(S.z_r,3));
        Fr = scatteredInterpolant(S.lon_rho(:),S.lat_rho(:),-S.h(:));
        for i = 1:size(lonr,1)
            for j = 1:size(lonr,2)
                h1 = Fr(lonr(i,j),latr(i,j));
                %h1 = interp2(S.lon_rho',S.lat_rho',-S.h',lonr(i,j),latr(i,j));
                z_br1 = squeeze(z_br{ib}(i,j,:));
                Tempb1 = squeeze(Tempb(i,j,:));
                Saltb1 = squeeze(Saltb(i,j,:));
                ikeep = find(~isnan(Tempb1));
                depth_int1 = [Pz(ikeep);h1];
                Tempb_int1 = [Tempb1(ikeep);btemp(i,j)];
                Saltb_int1 = [Saltb1(ikeep);Saltb1(ikeep(end))];
                %                 temp(i,j,:) = interp1(Pdepth(1:zind),squeeze(Tempb(i,j,:)),squeeze(z_br{ib}(i,j,:)));
                %                 salt(i,j,:) = interp1(Pdepth(1:zind),squeeze(Saltb(i,j,:)),squeeze(z_br{ib}(i,j,:)));
                temp(i,j,:) = interp1(depth_int1(:),Tempb_int1(:),z_br1(:));
                salt(i,j,:) = interp1(depth_int1(:),Saltb_int1(:),z_br1(:));
            end
        end
        Tb{ib} = squeeze(temp);
        Sb{ib} = squeeze(salt);

        % interpolate velocities in the vertical direction
        Urho = zeros(size(lonu,1),size(lonu,2),size(S.z_r,3));
        Vrho = zeros(size(lonu,1),size(lonu,2),size(S.z_r,3));
        for i = 1:size(lonu,1)
            for j = 1:size(lonu,2)
                h1 = Fr(lonu(i,j),latu(i,j));
                %h1 = interp2(S.lon_rho',S.lat_rho',-S.h',lonu(i,j),latu(i,j));
                z_bu1 = squeeze(z_bu{ib}(i,j,:));
                Uvelb1 = squeeze(Uvelb(i,j,:));
                Vvelb1 = squeeze(Vvelb(i,j,:));
                ikeep = find(~isnan(Uvelb1));
                depth_int1 = [Pz(ikeep);h1];
                Uvelb_int1 = [Uvelb1(ikeep);0];
                Vvelb_int1 = [Vvelb1(ikeep);0];
                %                 Urho(i,j,:) = interp1(Pdepth(1:zind),squeeze(Uvelb(i,j,:)),squeeze(z_bu{ib}(i,j,:)));
                %                 Vrho(i,j,:) = interp1(Pdepth(1:zind),squeeze(Vvelb(i,j,:)),squeeze(z_bu{ib}(i,j,:)));
                Urho(i,j,:) = interp1(depth_int1(:),Uvelb_int1(:),z_bu1(:));
                Vrho(i,j,:) = interp1(depth_int1(:),Vvelb_int1(:),z_bu1(:));
            end
        end

        %  Process velocity: rotate and/or average to staggered C-grid locations.

        Urot= Urho.*cos(angle_bu{ib})+Vrho.*sin(angle_bu{ib});
        Vrot=-Urho.*sin(angle_bu{ib})+Vrho.*cos(angle_bu{ib});

        if ib == 1 || ib == 2
            Ub{ib} = squeeze(0.5.*(Urot(1,:,:)+Urot(2,:,:)));
            Vb{ib} = squeeze(0.5.*(Vrot(1,1:end-1,:)+Vrot(1,2:end,:)));
        elseif ib == 3 || ib == 4
            Ub{ib} = squeeze(0.5.*(Urot(1:end-1,1,:)+Urot(2:end,1,:)));
            Vb{ib} = squeeze(0.5.*(Vrot(:,1,:)+Vrot(:,2,:)));
        end

        % Compute barotropic velocities by vertically integrating (u,v).
        Du = 0;
        Dv = 0;
        if ib == 1 || ib == 2
            Ubarb{ib} = zeros(S.Mm+2,1);
            Vbarb{ib} = zeros(S.Mm+1,1);
            for k = 1:S.N
                Duk = squeeze(0.5*(Hz_bu{ib}(1,:,k)+Hz_bu{ib}(2,:,k)));
                Du = Du+Duk;
                Ubarb{ib}(:) = Ubarb{ib}+Duk(:).*squeeze(Ub{ib}(:,k));

                Dvk = squeeze(0.5*(Hz_bu{ib}(1,1:end-1,k)+Hz_bu{ib}(1,2:end,k)));
                Dv = Dv+Dvk;
                Vbarb{ib}(:) = Vbarb{ib}+Dvk(:).*squeeze(Vb{ib}(:,k));
            end
            Ubarb{ib} = Ubarb{ib}(:)./Du(:);
            Vbarb{ib} = Vbarb{ib}(:)./Dv(:);
        elseif ib == 3 || ib == 4
            Ubarb{ib} = zeros(S.Lm+1,1);
            Vbarb{ib} = zeros(S.Lm+2,1);
            for k = 1:S.N
                Duk = squeeze(0.5*(Hz_bu{ib}(1,1:end-1,k)+Hz_bu{ib}(1,2:end,k)));
                Du = Du+Duk;
                Ubarb{ib}(:) = Ubarb{ib}+Duk(:).*squeeze(Ub{ib}(:,k));

                Dvk = squeeze(0.5*(Hz_bu{ib}(:,1,k)+Hz_bu{ib}(:,2,k)));
                Dv = Dv+Dvk;
                Vbarb{ib}(:) = Vbarb{ib}+Dvk(:).*squeeze(Vb{ib}(:,k));
            end
            Ubarb{ib}(:) = Ubarb{ib}(:)./Du(:);
            Vbarb{ib}(:) = Vbarb{ib}(:)./Dv(:);
        end
    end

    % write boundary conditiions into the netcdf file
    if (WRITE)

        BryRec = BryRec+1;

        [status]=nc_write(BRYname, 'bry_time', S.bry_time, BryRec);

        if (S.boundary(1))
            [status]=nc_write(BRYname, 'zeta_west' , FSb{1}, BryRec);
            [status]=nc_write(BRYname, 'ubar_west' , Ubarb{1}, BryRec);
            [status]=nc_write(BRYname, 'vbar_west' , Vbarb{1}, BryRec);
            [status]=nc_write(BRYname, 'u_west',     Ub{1},    BryRec);
            [status]=nc_write(BRYname, 'v_west',     Vb{1},    BryRec);
            [status]=nc_write(BRYname, 'temp_west' , Tb{1}, BryRec);
            [status]=nc_write(BRYname, 'salt_west' , Sb{1}, BryRec);
        end
        if (S.boundary(2))
            [status]=nc_write(BRYname, 'zeta_east' , FSb{2}, BryRec);
            [status]=nc_write(BRYname, 'ubar_east' , Ubarb{2}, BryRec);
            [status]=nc_write(BRYname, 'vbar_east' , Vbarb{2}, BryRec);
            [status]=nc_write(BRYname, 'u_east',     Ub{2},    BryRec);
            [status]=nc_write(BRYname, 'v_east',     Vb{2},    BryRec);
            [status]=nc_write(BRYname, 'temp_east' , Tb{2}, BryRec);
            [status]=nc_write(BRYname, 'salt_east' , Sb{2}, BryRec);
        end
        if (S.boundary(3))
            [status]=nc_write(BRYname, 'zeta_south' , FSb{3}, BryRec);
            [status]=nc_write(BRYname, 'ubar_south' , Ubarb{3}, BryRec);
            [status]=nc_write(BRYname, 'vbar_south' , Vbarb{3}, BryRec);
            [status]=nc_write(BRYname, 'u_south',     Ub{3},    BryRec);
            [status]=nc_write(BRYname, 'v_south',     Vb{3},    BryRec);
            [status]=nc_write(BRYname, 'temp_south' , Tb{3}, BryRec);
            [status]=nc_write(BRYname, 'salt_south' , Sb{3}, BryRec);
        end
        if (S.boundary(4))
            [status]=nc_write(BRYname, 'zeta_north' , FSb{4}, BryRec);
            [status]=nc_write(BRYname, 'ubar_north' , Ubarb{4}, BryRec);
            [status]=nc_write(BRYname, 'vbar_north' , Vbarb{4}, BryRec);
            [status]=nc_write(BRYname, 'u_north',     Ub{4},    BryRec);
            [status]=nc_write(BRYname, 'v_north',     Vb{4},    BryRec);
            [status]=nc_write(BRYname, 'temp_north' , Tb{4}, BryRec);
            [status]=nc_write(BRYname, 'salt_north' , Sb{4}, BryRec);
        end

    end

end


