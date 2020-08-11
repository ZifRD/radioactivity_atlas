#Author: Zaporozhtsev I.F.
#Created: 2019-2020

library(DBI)

Sys.setlocale("LC_ALL", "Russian")

conn <- dbConnect( RPostgres::Postgres(),
                     host = 'localhost',
                     dbname = '###',
                     port = 5432,
                     user = '###',
                     password = '###');  
  
regionName0 <- "Баренцево море"
shipName0 <- "НИС Дальние Зеленцы"
pollName0 <- "Cs137"
objectName0 <- "вода"  

getPoints0 <- function(){
  tableName <- "w_samples";
  query1=paste0("SELECT stations.st_date as date1,expeditions.start_date as date2,expeditions.stop_date as date3,
                EXTRACT(YEAR FROM to_date(expeditions.start_date,'dd/mm/yyyy')) as year,expeditions.exp_id as expID,
                ships.name as shipName,regions.name as regName,stations.reg_comment as regCom,
                stations.lat as lat,stations.lon as lon,stations.station_id as stid,
                pollutants.name as polName,",tableName,".c as polSign,",tableName,".val as polVal,",tableName,".err as polErr
                FROM stations
                INNER JOIN regions ON stations.region_id=regions.region_id
                INNER JOIN expeditions ON expeditions.exp_id = stations.exp_id
                INNER JOIN ships ON expeditions.ship_id=ships.ship_id
                INNER JOIN w_samples ON stations.station_id=w_samples.station_id
                INNER JOIN pollutants ON pollutants.pollutant_id=w_samples.pollutant_id
                WHERE regions.name = '",regionName0,"' AND ships.name = '",
                shipName0,"' AND pollutants.name ='",pollName0,"';");
  res1 <- dbSendQuery(conn, query1);
  on.exit(dbClearResult(res1));
  mydf <- dbFetch(res1)
  return(mydf)
}
getPoints <- function(objectWSB,date2,date3,region,ship,pollutant){
  
  tableName <- "";
  if (objectWSB == "вода") tableName <- "w_samples";
  if (objectWSB == "грунт") tableName <- "s_samples";
  if (objectWSB == "биота") tableName <- "b_samples";

  query1=paste0("SELECT stations.st_date as date1,expeditions.start_date as date2,expeditions.stop_date as date3,
    EXTRACT(YEAR FROM to_date(expeditions.start_date,'dd/mm/yyyy')) as year,expeditions.exp_id as expID,
                ships.name as shipName,regions.name as regName,stations.reg_comment as regCom,
                stations.lat as lat,stations.lon as lon,stations.station_id as stID,
                pollutants.name as polName,",tableName,".c as polSign,",tableName,".val as polVal,",tableName,".err as polErr
                FROM stations
                INNER JOIN regions ON stations.region_id=regions.region_id
                INNER JOIN expeditions ON expeditions.exp_id = stations.exp_id
                INNER JOIN ships ON expeditions.ship_id=ships.ship_id
                INNER JOIN ",tableName," ON stations.station_id=",tableName,".station_id
                INNER JOIN pollutants ON pollutants.pollutant_id=",tableName,".pollutant_id ", 
                "WHERE to_date(expeditions.start_date,'dd/mm/yyyy') >= to_date('",
                   date2,"','yyyy-mm-dd')",
                "AND to_date(expeditions.stop_date,'dd/mm/yyyy') <= to_date('",
                  date3,"','yyyy-mm-dd')")
  
  regionStr <- ifelse(region == "Все","",paste0("regions.name = '",region,"'"));
  shipStr <- ifelse(ship == "Все","",paste0("ships.name = '",ship,"'"));
  pollStr <- ifelse(pollutant == "Все","",paste0("pollutants.name = '",pollutant,"'"));

  if (nchar(paste0(regionStr,shipStr,pollStr)) > 0)  #at least one in not null
  {
    query1 <- paste0(query1," AND ");
    if((regionStr != "") & (shipStr != "") & (pollStr != ""))
      query1 <- paste0(query1,regionStr," AND ",shipStr," AND ",pollStr)  #all not null
    else
    {
      if(!((nchar(paste0(regionStr,shipStr,pollStr)) == nchar(regionStr))
              | (nchar(paste0(regionStr,shipStr,pollStr)) == nchar(shipStr))
              | (nchar(paste0(regionStr,shipStr,pollStr)) == nchar(pollStr))))
      {
        if(regionStr != "")
          query1 <- paste0(query1,regionStr," AND ",shipStr,pollStr)
        else
          query1 <- paste0(query1,shipStr," AND ",pollStr);
      }
      else query1 <- paste0(query1,regionStr,shipStr,pollStr);   #only one is not null
    }
  }

  query1 <- paste0(query1,";");
  res1 <- dbGetQuery(conn, query1);
  return(res1);
}

getOriginExcelTable <- function(){ 
  query1=paste0("SELECT 
    stations.st_date,expeditions.start_date,expeditions.stop_date,
    EXTRACT(YEAR FROM to_date(expeditions.start_date,'dd/mm/yyyy')),ships.name,regions.name,
    stations.reg_comment,stations.lat,stations.lon,
    pollutants.name,w_samples.c,w_samples.val,w_samples.err
    FROM expeditions 
                INNER JOIN stations ON stations.exp_id=expeditions.exp_id
                INNER JOIN w_samples ON w_samples.station_id=stations.station_id
                INNER JOIN pollutants ON w_samples.pollutant_id=pollutants.pollutant_id
                INNER JOIN regions ON stations.region_id=regions.region_id
                INNER JOIN ships ON expeditions.ship_id=ships.ship_id 
                GROUP BY expeditions.exp_id,stations.station_id,w_samples.sample_id,pollutants.pollutant_id,regions.region_id,ships.ship_id ORDER BY TO_DATE(expeditions.start_date, 'DD/MM/YYYY') ASC;");
  res1 <- dbGetQuery(conn, query1);
  res1 = as.data.frame(res1);
  colnames(res1)[1] <- 'Дата1';
  colnames(res1)[2] <- 'Дата2';
  colnames(res1)[3] <- 'Дата3';
  colnames(res1)[4] <- 'Год';
  colnames(res1)[5] <- 'Судно';
  colnames(res1)[6] <- 'Район';
  colnames(res1)[7] <- 'Подрайон';
  colnames(res1)[8] <- 'Широта';
  colnames(res1)[9] <- 'Долгота';
  colnames(res1)[10] <- 'Поллютант';
  colnames(res1)[11] <- 'Знак';
  colnames(res1)[12] <- 'Значение';
  colnames(res1)[13] <- 'Погрешность';
  return(res1)
}

getPollutantsTable <- function(){ 
  query1=paste0("SELECT * FROM pollutants;")
  res1 <- dbSendQuery(conn, query1);
  on.exit(dbClearResult(res1));
  dbFetch(res1)
}


