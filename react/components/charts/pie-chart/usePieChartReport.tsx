import {
  useEffect, useState, useCallback,
} from 'react';
import { axios } from '@choerodon/boot';
import { uniqBy } from 'lodash';
import {
  PieChartProps, IPieChartType, IPieData, IDimension,
} from '@/components/charts/pie-chart';
import { PieSearchProps } from '@/components/charts/pie-chart/search';
import {
  pageConfigApi,
  reportApi, sprintApi, statusApi, versionApi,
} from '@/api';
import { ISprint, IStatus, IVersion } from '@/common/types';
import useControlledDefaultValue from '@/hooks/useControlledDefaultValue';
import { getProjectId } from '@/utils/common';

export interface PieConfig {
  type?: IPieChartType
  chooseId?: string,
  chooseDimension?: IDimension,
  projectId?: string,
}

export interface IType {
  isCustom?: boolean, value: string, title: string
}

export const types = [
  { title: '经办人', value: 'assignee' },
  { title: '模块', value: 'component' },
  { title: '工作项类型', value: 'typeCode' },
  { title: '版本', value: 'version' },
  { title: '优先级', value: 'priority' },
  { title: '状态', value: 'status' },
  { title: '冲刺', value: 'sprint' },
  { title: '史诗', value: 'epic' },
  { title: '标签', value: 'label' },
  { title: '报告人', value: 'reporter' },
  { title: '主要负责人', value: 'mainResponsible' },
  { title: '环境', value: 'environment' },
  { title: '参与人', value: 'participant' },
];

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
  const [status, setStatusList] = useState<IStatus[]>([]);
  const [allTypes, setAllTypes] = useState<IType[]>(types);

  const loadSprintsAndVersions = useCallback(() => {
    axios.all([
      sprintApi.project(projectId).loadSprints(['started', 'closed']),
      versionApi.project(projectId).loadNamesByStatus(),
      statusApi.project(projectId).loadByProject('agile'),
      pageConfigApi.load(),
    ])
      .then(axios.spread((sprintList: ISprint[], versionList: IVersion[], statusList: IStatus[], fields: { content: any[]}) => {
        setSprints(sprintList);
        setVersions(versionList);
        setStatusList(statusList);
        setAllTypes(uniqBy([...types, ...(fields.content || []).filter((item) => !item.system
        && ['single', 'multiple', 'checkbox', 'radio', 'member', 'multiMember'].includes(item.fieldType)).map((item) => ({
          title: item.name,
          value: item.id,
          isCustom: true,
        }))], 'value'));
      }));
  }, [projectId]);

  const loadData = useCallback(async () => {
    const currentType = allTypes.find((item:IType) => item.value === type);
    if (currentType) {
      setLoading(true);
      reportApi.project(projectId).loadPie(currentType, chooseDimension === 'sprint' ? chooseId : '', chooseDimension === 'version' ? chooseId : '', chooseDimension === 'status' ? chooseId : '')
        .then((res: IPieData[]) => {
          const len = res.length;
          if (len) {
            const initColors = ['#9665E2', '#F0657D', '#FAD352', '#FF9915', '#45A3FC', '#5365EA', '#47CBCA', '#59CB79', '#F953BA', '#D3D3D3'];
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
    }
  }, [allTypes, chooseDimension, chooseId, onFinish, projectId, type]);

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
    status,
    chooseDimension,
    setType,
    setChooseDimension,
    setChooseId,
    projectId,
    allTypes,
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
    allTypes,
  };
  return [searchProps, props];
}

export default usePieChartReport;
