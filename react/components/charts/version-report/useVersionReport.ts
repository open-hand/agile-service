import { useState, useEffect, useCallback } from 'react';
import { reportApi, versionApi } from '@/api';
import useControlledDefaultValue from '@/hooks/useControlledDefaultValue';
import { getProjectId } from '@/utils/common';
import { VersionReportProps, IVersionReportChart, IVersionReportTable } from './index';
import { VersionReportSearchProps, IVersion } from './search';
import { IUnit } from '../iteration-speed/search';
import { IChartSearchHookAdditionalConfig } from '../types.';
import useDataSet from '../useGetChartSearchDataSet';
import tree from '@/components/tree';
import useGetChartSearchDataSet from '../useGetChartSearchDataSet';

export interface VersionReportConfig extends IChartSearchHookAdditionalConfig {
  unit?: IUnit,
  versionId?: string
  projectId?: string
}

const useVersionReport = (config?: VersionReportConfig, onFinish?: Function): [VersionReportProps, VersionReportSearchProps] => {
  const projectId = config?.projectId || getProjectId();
  const [loading, setLoading] = useState<boolean>(false);
  const [unit, setUnit] = useControlledDefaultValue<IUnit>(config?.unit || 'story_point');
  const [versions, setVersions] = useState<IVersion[]>([]);
  const [versionId, setVersionId] = useControlledDefaultValue<string>(config?.versionId || '');
  const [data, setData] = useState<IVersionReportChart[]>([]);
  const [tableData, setTableData] = useState<IVersionReportTable[]>([]);

  const loadVersions = useCallback(() => {
    setLoading(true);
    versionApi.project(projectId).loadNamesByStatus(['version_planning', 'released'])
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
  }, [config?.versionId, projectId, setVersionId]);

  const loadTableData = useCallback(() => {
    if (versionId) {
      setLoading(true);
      reportApi.project(projectId).loadVersionTable(versionId).then((res: IVersionReportTable[]) => {
        setTableData(res);
        setLoading(false);
      });
    }
  }, [projectId, versionId]);

  const loadData = useCallback(() => {
    if (versionId) {
      setLoading(true);
      reportApi.project(projectId).loadVersionChart(versionId, unit)
        .then((res: IVersionReportChart[]) => {
          setData(res);
          setLoading(false);
          onFinish && setTimeout(onFinish);
        });
    }
  }, [onFinish, projectId, unit, versionId]);

  useEffect(() => {
    loadVersions();
  }, [loadVersions]);

  useEffect(() => {
    loadData();
  }, [loadData]);

  useEffect(() => {
    loadTableData();
  }, [loadTableData]);

  const searchDataSet = useGetChartSearchDataSet({
    fields: [
      { name: 'version', label: '版本', required: true },
      { name: 'unit', label: '单位', required: true },
    ],
    enabled: config?.openValidate,
    valueChangeDataSetValue: {
      version: versionId,
      unit,
    },
  });
  const props: VersionReportProps = {
    loading, data, tableData, unit,
  };
  const searchProps: VersionReportSearchProps = {
    unit, setUnit, versions, versionId, setVersionId, projectId, searchDataSet,
  };
  return [props, searchProps];
};

export default useVersionReport;
