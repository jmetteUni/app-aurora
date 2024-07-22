% This program is used to create the netcdf forcing file containing bottom
% fluxes of tracers (e.g., temperature, salinity, dye, etc.)
%created by G.Xu
%modified by J. Mette, 20/04/2024
clc;
clear;
close all;

%% Initialization
% Path to ROMS grid file
%griddir = '/home/jonathan/Dokumente/model/files_guangyu/model_file_EPR/input';
%gridname = 'EPR_grid_coarse.nc';
appdir = '/home/jonathan/Dokumente/model/roms_project/aurora/matlab';
%griddir = '/home/jonathan/Dokumente/model/inputs/Gridbuilder';
gridname = 'Grid_Aurora_M64L64.nc';
grid = fullfile(appdir,gridname);
% Base date for ROMS forcing files
mybasedate = datenum(2022,07,26,0,0,0);
dates = datenum(2022,07,26,0,0,0);
datee = datenum(2023,07,27,0,0,0);
frc_date = dates:3:datee;
eruption_date = dates;
heat_vent = 90000000; % Aurora, arbitrary   %from poster:20MW %from Wegener2023: 130MW
mass_dye1 = 10;

compute_heat = 1; % toggle for computing heat flux
isplot = 0; % toggle for plotting bottom heat flux
iscoarse = 1;
if iscoarse == 0
    coarsefine = 'fine';
else
    coarsefine = '';
end

% Path to output forcing files
%outdir = 'F:\projects\ocean_modeling\ROMS\Endeavour\cruise_2023\input';
%outdir = '/home/jonathan/Dokumente/model/boundary_conditions';
outdir = appdir;
if ~exist(outdir,'dir')
    mkdir(outdir)
end

% load ROMS grid
G = get_roms_grid(grid,grid);
roms_lon = G.lon_rho;
roms_lat = G.lat_rho;
pm = G.pm;
pn = G.pn;
dlon = min(diff(roms_lon(:,1)));
dlat = min(diff(roms_lat(1,:)));
bathy = G.h;

% convert lon/lat to x/y
dczone = utmzone(mean(roms_lat(:)),mean(roms_lon(:)));
utmstruct = defaultm('utm');
utmstruct.zone = dczone;
utmstruct.geoid = wgs84Ellipsoid;
utmstruct = defaultm(utmstruct);
[roms_x,roms_y] = mfwdtran(utmstruct,roms_lat,roms_lon);

% Various parameters.
spherical = true;                       % Spherical switch
nctype    = 'nc_float';                 % Input data is in single precision
Unlimited = true;                       % time dimension is umlimited in
mode = netcdf.getConstant('CLOBBER');                    % overwrite!!!
mode = bitor(mode,netcdf.getConstant('64BIT_OFFSET'));


%% create the nc file    %bhflux: heat flux, bwflux: salt flux
F(1).output = sprintf('%s_bhflux_%dMW%s_%s.nc',gridname(1:end-3),heat_vent,coarsefine,datestr(eruption_date,'yyyymmddTHH'));
F(1).Vname = 'bhflux';
F(1).Tname = 'bhf_time';
%F(2).output = sprintf('%s_bwflux_Kellog_120day_1_vent.nc',gridname(1:end-3));
%F(2).Vname = 'bwflux';
%F(2).Tname = 'bwf_time';
F(2).output = sprintf('%s_bpflux_%s.nc',gridname(1:end-3),datestr(eruption_date,'yyyymmddTHH'));
F(2).Vname = 'dye_01_bflux';
F(2).Tname = 'ocean_time';
for n = 1:length(F)
    ncname = char(F(n).output);
    Vname  = char(F(n).Vname);
    Tname  = char(F(n).Tname);
    disp(blanks(1));
    disp(['** Creating ROMS NetCDF forcing file: ', ncname,' **']);
    disp(blanks(1))

    S.Filename = fullfile(outdir,ncname);

    %     Im = 600;
    %     Jm = 600;
    %     lon = linspace(min(roms_lon(:))-0.1,max(roms_lon(:))+0.1,Im);
    %     lat = linspace(min(roms_lat(:))-0.1,max(roms_lat(:))+0.1,Jm);

    %     lon = min(roms_lon(:))-0.1:dlon:max(roms_lon(:))+0.1;
    %     lat = min(roms_lat(:))-0.1:dlat:max(roms_lat(:))+0.1;
    %     Im = length(lon);
    %     Jm = length(lat);

    %     llon = repmat(lon(:),[1 Jm]);
    %     llat = repmat(lat(:)', [Im 1]);

    Im = size(roms_lon,1);
    Jm = size(roms_lat,2);

    lon = roms_lon(:,1);
    lat = roms_lat(1,:);

    llon = roms_lon;
    llat = roms_lat;

    S.Attributes(1).Name      = 'type';
    S.Attributes(1).Value     = 'FORCING file';

    S.Attributes(2).Name      = 'title';
    S.Attributes(2).Value     = sprintf('%s for Axial simulation',Vname);

    S.Attributes(3).Name      = 'history';
    S.Attributes(3).Value     = ['Forcing file created with ',            ...
        which(mfilename) ' on ' date_stamp];

    S.Dimensions(1).Name      = 'lon';
    S.Dimensions(1).Length    = Im;
    S.Dimensions(1).Unlimited = false;

    S.Dimensions(2).Name      = 'lat';
    S.Dimensions(2).Length    = Jm;
    S.Dimensions(2).Unlimited = false;

    S.Dimensions(3).Name      = Tname;
    S.Dimensions(3).Length    = nc_constant('nc_unlimited');
    S.Dimensions(3).Unlimited = true;

    S.Variables(1) = roms_metadata('spherical');
    S.Variables(2) = roms_metadata('lon');
    S.Variables(3) = roms_metadata('lat');
    S.Variables(4) = roms_metadata(Tname, [], [], Unlimited);
    S.Variables(5) = roms_metadata(Vname, spherical, nctype, Unlimited);

    % Edit the time variable 'units" attribute for the correct reference
    % time and add calendar attribute.
    natts = length(S.Variables(4).Attributes);
    iatt  = strcmp({S.Variables(4).Attributes.Name}, 'units');
    S.Variables(4).Attributes(iatt).Value = ['days since ' datestr(mybasedate,31)];

    S.Variables(4).Attributes(natts+1).Name  = 'calendar';
    S.Variables(4).Attributes(natts+1).Value = 'gregorian';

    % Check ROMS metadata structure.  Fill unassigned fields.
    S = check_metadata(S);
    ncid = nc_create(S.Filename, mode, S); % create a new NetCDF file
    status = nc_write(S.Filename, 'spherical', int32(spherical));
    status = nc_write(S.Filename, 'lon',       llon);
    status = nc_write(S.Filename, 'lat',       llat);

end

%% Populate forcing variables
frc_time = frc_date-eruption_date;
if compute_heat == 0
    bhflux = zeros(length(lon),length(lat),length(frc_time));
    %bwflux = zeros(length(lon),length(lat),length(frc_time));
    bpflux = zeros(length(lon),length(lat),length(frc_time));
else
    % vent field coordinates (north to south: Sasquatch, Salty Dawg, High Rise,
    % Main Endeavour, Mothra
    %lon_vent = [-129.0662,-129.0756,-129.0894,-129.0981,-129.1082];
    %lat_vent = [47.9969,47.9822,47.9666,47.9487,47.9233];
    %Aurora 82°53.83'N, 6°15.32'W
    lon_vent = -6.2553333;
    lat_vent = 82.8971667;
    [lat_vent,isort] = sort(lat_vent);
    lon_vent = lon_vent(isort);
    [x_vent,y_vent] = projfwd(utmstruct,lat_vent,lon_vent);

    % heat flux of individual vent fields
    %heat_vent = [300 300 600 600 300]; % MW
    %heat_vent = 900*[0.15,0.34,0.43 0.075 0.005]; % MW ( Kellog Thesis 2011 )
    %heat_vent = [900*0.15,380,556,900*0.075,900*0.005]; % MW (Endeavour Cruise 2023 Single Cast)
    %heat_vent = [900*0.15,850,1500,900*0.075,900*0.005]; % MW (Endeavour Cruise 2023 YoYo)
    %heat_vent = [900*0.15,1300,1800,900*0.075,900*0.005]; % MW (Endeavour Cruise 2023 increased heat fluxes for MEF and HR)
    %heat_vent = [900*0.15,1500,1800,900*0.075,900*0.005]; % MW (Endeavour Cruise 2023 further increased heat flux for MEF)
    %heat_vent = [900*0.15,1800,1800,900*0.075,900*0.005]; % MW (Endeavour Cruise 2023 further increased heat flux for MEF)
    %heat_vent = 20; % Aurora, arbitrary   %form poster:20MW

    % construct bottom source variables for individual vent fields
    bhflux = zeros(length(lon),length(lat),length(frc_time));
    %bwflux = zeros(length(lon),length(lat),length(frc_time));
    bpflux = zeros(length(lon),length(lat),length(frc_time));
    if iscoarse
        ix_vent = zeros(1,length(lon_vent));
        iy_vent = zeros(1,length(lon_vent));
    else
        ix_vent = zeros(3,length(lon_vent));
        iy_vent = zeros(3,length(lon_vent));
    end
    for ivent = 1:length(lon_vent)
        x1 = x_vent(ivent);
        y1 = y_vent(ivent);
        heat1 = heat_vent(ivent)/size(ix_vent,1)/size(iy_vent,1);
        [~,ind] = min(sqrt((roms_x(:)-x1).^2+(roms_y(:)-y1).^2));
        [is,js] = ind2sub(size(llon),ind);
        if iscoarse
            ix_vent(1,ivent) = is;
            iy_vent(1,ivent) = js;
        else
            ix_vent(:,ivent) = is-1:is+1;
            iy_vent(:,ivent) = js-1:js+1;
        end
        for ix = 1:size(ix_vent,1)
            for iy = 1:size(iy_vent,1)
                is1 = ix_vent(ix,ivent);
                js1 = iy_vent(iy,ivent);
                cell_area = 1/pm(is1,js1)*1/pn(is1,js1); % area of the source grid cell
                bhflux1 = heat1*10^6/cell_area*ones(1,length(frc_time));
                bhflux1(frc_time<0) = 0;
                bhflux(is1,js1,:) = bhflux1;
                bp_max = mass_dye1      *heat_vent(ivent)/max(heat_vent);  %bp_max is 10*the heat of one vent? 
                bpflux1 = bp_max*ones(1,length(frc_time));  %extend bp_max to time axis
                bpflux1(frc_time<0) = 0;    %set bpflux to 0 for time<0
                bpflux(is1,js1,:) = bpflux1;    %extend bpflux spatially
                % bw_max = 0*heat_vent(ivent)/max(heat_vent);
                % bwflux1 = bbpw_max*ones(1,length(frc_time));
                % bwflux1(frc_time<0) = 0;
                % bwflux(is1,js1,:) = bwflux1;
            end
        end
    end
    bhflux = -bhflux;
    bpflux = -bpflux;
    %bwflux = -bwflux;
end

% plot heat sources on the map
if isplot == 1
    lonlim = [min(lon_vent(:))-0.1,max(lon_vent(:))+0.1];
    latlim = [min(lat_vent(:))-0.1,max(lat_vent(:))+0.1];
    figure(1)
    set(gcf,'Position',[100 100 500 600])
    R = 1;
    lon_r_plot = roms_lon(1:R:end,1:R:end);
    lat_r_plot = roms_lat(1:R:end,1:R:end);
    bhflux_plot = -squeeze(bhflux(1:R:end,1:R:end,end));
    m_proj('utm','lon',lonlim,'lat',latlim);
    m_pcolor(lon_r_plot,lat_r_plot,bhflux_plot);
    m_grid('linewi',2,'tickdir','out','fontsize',14)
    ct = cmocean('thermal');
    colormap(ct);
    clim([0 500])
    shading flat;
    h = colorbar;
    title(h,'Watt')
    yt = get(h,'ytick');
    set(h,'ytick',yt(1:2:end));
    hold on;
    m_contour(roms_lon,roms_lat,-bathy,-2400:50:-2000,'w')
    hold off;
    set(gca,'fontsize',14);
end

% write data into forcing files
F(1).field = single(bhflux);
%clear bhflux
%F(2).field = single(bwflux);
% clear bwflux
F(2).field = single(bpflux);
%clear bpflux
for n = 1:length(F)
    OutFile = fullfile(outdir,char(F(n).output));
    Troms   = char(F(n).Tname);
    Vroms   = char(F(n).Vname);
    Field = F(n).field;
    % Write out data
    %status = nc_write(OutFile, Troms, frc_date-mybasedate);
    ncwrite(OutFile, Troms, frc_date-mybasedate);
    %status = nc_write(OutFile, Vroms, Field);
    ncwrite(OutFile, Vroms, Field);
end


%% compute total heat flux
% cell_area = (1./pm).*(1./pn);
% heat_flux = squeeze(sum(bhflux.*repmat(cell_area,[1,1,length(frc_time)]),[1,2]));

% plot source locations
% figure
% pcolorjw((roms_x-x0)/1000,(roms_y-y0)/1000,h);
% caxis([-2800,-2000]);
% colormap('jet');
% axis image;
% hold on;
% for ivent = 1:length(lon_vent)
%     indx = ix_vent(:,ivent);
%     indy = iy_vent(:,ivent);
%     plot((roms_x(indx,indy)-x0)/1000,(roms_y(indx,indy)-y0)/1000,'.k');
% end
% hold off;
% xlim([-20,20]);
% ylim([-20 20]);



