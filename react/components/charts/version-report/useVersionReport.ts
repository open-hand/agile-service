import { useState, useEffect, useCallback } from 'react';
import { reportApi, versionApi } from '@/api';
import VersionReport, { VersionReportProps, IVersionReportChart, IVersionReportTable } from './index';
import VersionReportSearch, { VersionReportSearchProps, IVersion } from './search';
import { IUnit } from '../iteration-speed/search';

const useVersionReport = (): [VersionReportProps, VersionReportSearchProps] => {
  const [loading, setLoading] = useState<boolean>(false);
  const [unit, setUnit] = useState<IUnit>('story_point');
  const [versions, setVersions] = useState<IVersion[]>([]);
  const [versionId, setVersionId] = useState<string>('');
  const [data, setData] = useState<IVersionReportChart[]>([]);
  const [tableData, setTableData] = useState<IVersionReportTable[]>([]);

  const loadVersions = useCallback(() => {
    setLoading(true);
    versionApi.loadNamesByStatus(['version_planning', 'released'])
      .then((res: IVersion[]) => {
        setLoading(false);
        setVersions(res);
        setVersionId(res.length ? res[0].versionId : '');
      });
  }, []);

  const loadTableData = useCallback(() => {
    if (versionId) {
      setLoading(true);
      reportApi.loadVersionTable(versionId).then((res: IVersionReportTable[]) => {
        setTableData(res);
        setLoading(false);
      });
    }
  }, [versionId]);

  const loadData = useCallback(() => {
    if (versionId) {
      setLoading(true);
      reportApi.loadVersionChart(versionId, unit)
        .then((res: IVersionReportChart[]) => {
          setData(res);
          setLoading(false);
        });
    }
  }, [unit, versionId]);

  useEffect(() => {
    loadVersions();
  }, [loadVersions]);

  useEffect(() => {
    loadData();
  }, [loadData]);

  useEffect(() => {
    loadTableData();
  }, [loadTableData]);

  const props: VersionReportProps = {
    loading, data, tableData, unit,
  };
  const searchProps: VersionReportSearchProps = {
    unit, setUnit, versions, versionId, setVersionId,
  };
  return [props, searchProps];
};

export default useVersionReport;
