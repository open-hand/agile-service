import { useState, useEffect, useCallback } from 'react';
import { reportApi, versionApi } from '@/api';
import useControlledDefaultValue from '@/hooks/useControlledDefaultValue';
import { VersionReportProps, IVersionReportChart, IVersionReportTable } from './index';
import { VersionReportSearchProps, IVersion } from './search';
import { IUnit } from '../iteration-speed/search';

interface VersionReportConfig {
  unit: IUnit,
  versionId: string
}

const useVersionReport = (config?: VersionReportConfig): [VersionReportProps, VersionReportSearchProps] => {
  const [loading, setLoading] = useState<boolean>(false);
  const [unit, setUnit] = useControlledDefaultValue<IUnit>(config?.unit || 'story_point');
  const [versions, setVersions] = useState<IVersion[]>([]);
  const [versionId, setVersionId] = useControlledDefaultValue<string>(config?.versionId || '');
  const [data, setData] = useState<IVersionReportChart[]>([]);
  const [tableData, setTableData] = useState<IVersionReportTable[]>([]);

  const loadVersions = useCallback(() => {
    setLoading(true);
    versionApi.loadNamesByStatus(['version_planning', 'released'])
      .then((res: IVersion[]) => {
        let initVersionId = '';
        setLoading(false);
        setVersions(res);
        if (config?.versionId) {
          initVersionId = config.versionId;
        } else {
          initVersionId = res.length ? res[0].versionId : '';
        }
        setVersionId(initVersionId);
      });
  }, [config?.versionId, setVersionId]);

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
