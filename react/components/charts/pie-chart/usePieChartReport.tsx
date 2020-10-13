import {
  useEffect, useState, useCallback,
} from 'react';
import { axios } from '@choerodon/boot';
import {
  PieChartProps, IPieChartType, IPieData, IDimension,
} from '@/components/charts/pie-chart';
import { PieSearchProps } from '@/components/charts/pie-chart/search';
import { reportApi, sprintApi, versionApi } from '@/api';
import { ISprint, IVersion } from '@/common/types';
import useControlledDefaultValue from '@/hooks/useControlledDefaultValue';
import { getProjectId } from '@/utils/common';

export interface PieConfig {
  type?: IPieChartType
  chooseId?: string,
  chooseDimension?: IDimension,
  projectId?: string,
}

function usePieChartReport(config?: PieConfig, onFinish?: Function): [PieSearchProps, PieChartProps] {
  const projectId = config?.projectId || getProjectId();
  const [type, setType] = useControlledDefaultValue<IPieChartType>(config?.type || 'assignee');
  const [loading, setLoading] = useState(false);
  const [chooseId, setChooseId] = useControlledDefaultValue<string | ''>(config?.chooseId || '');
  const [chooseDimension, setChooseDimension] = useControlledDefaultValue<IDimension>(config?.chooseDimension || '');
  const [data, setData] = useState<IPieData[]>([]);
  const [colors, setColors] = useState<string[]>([]);
  const [sprints, setSprints] = useState<ISprint[]>([]);
  const [versions, setVersions] = useState<IVersion[]>([]);

  const loadSprintsAndVersions = useCallback(() => {
    axios.all([
      sprintApi.project(projectId).loadSprints(['started', 'closed']),
      versionApi.project(projectId).loadNamesByStatus(),
    ])
      .then(axios.spread((sprintList: ISprint[], versionList: IVersion[]) => {
        setSprints(sprintList);
        setVersions(versionList);
      }));
  }, [projectId]);

  const loadData = useCallback(async () => {
    setLoading(true);
    reportApi.project(projectId).loadPie(type, chooseDimension === 'sprint' ? chooseId : '', chooseDimension === 'version' ? chooseId : '')
      .then((res: IPieData[]) => {
        const len = res.length;
        if (len) {
          const initColors = ['#9665E2', '#F0657D', '#FAD352', '#FF9915', '#45A3FC', '#3F51B5', '#47CBCA', '#59CB79', '#F953BA', '#D3D3D3'];
          if (len > 10) {
            for (let i = 10; i < len; i += 1) {
              // eslint-disable-next-line no-bitwise
              initColors.push(`#${(`00000${((Math.random() * 16777215 + 0.5) >> 0).toString(16)}`).slice(-6)}`);
            }
          }
          setColors(initColors);
          setData(res);
        }
        setLoading(false);
        onFinish && setTimeout(onFinish);
      })
      .catch(() => {
        setLoading(false);
      });
  }, [chooseDimension, chooseId, onFinish, projectId, type]);

  useEffect(() => {
    loadData();
  }, [loadData]);

  useEffect(() => {
    loadSprintsAndVersions();
  }, [loadSprintsAndVersions]);

  const searchProps: PieSearchProps = {
    type,
    chooseId,
    sprints,
    versions,
    chooseDimension,
    setType,
    setChooseDimension,
    setChooseId,
    projectId,
  };
  const props: PieChartProps = {
    loading,
    type,
    data,
    colors,
    chooseDimension,
    chooseId,
    sprints,
    versions,
  };
  return [searchProps, props];
}

export default usePieChartReport;
