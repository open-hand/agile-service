import React, {
  useCallback, useEffect, useMemo, useState,
} from 'react';
import { DataSet } from 'choerodon-ui/pro';
import { observer } from 'mobx-react-lite';
import { unstable_batchedUpdates as batchedUpdates } from 'react-dom';
import Record from 'choerodon-ui/pro/lib/data-set/Record';
import { DataSetSelection } from 'choerodon-ui/dataset/data-set/enum';
import { statusTransformApi } from '@/api';
import { OldLoading as Loading } from '@/components/Loading';
import useFields from '@/routes/Issue/components/BatchModal/useFields';
import useSubProjects from '@/hooks/data/useSubProjects';
import ProjectList from './ProjectList';
import ProjectConfig from './ProjectConfig';
import styles from './index.less';

export interface IFeatureLinkageSetting {
  id: string
  issueTypeId: string
  projectId: string
  statusId: string
  type: 'anyone_transfer' | 'all_transfer',
}

interface IFieldK {
  key: number,
}

const FeatureLinkage = ({
  // @ts-ignore
  modal, record, issueTypeId, customCirculationDataSet, selectedTypeName,
}) => {
  const [loading, setLoading] = useState(false);
  const { data: subProjects } = useSubProjects({ onlySelectEnable: true });
  const [fields, Field] = useFields();
  const [currentProject, setCurrentProject] = useState<string | null>(null);
  const [linkagesMap, setLinkagesMap] = useState<Map<string, IFeatureLinkageSetting[]>>(new Map());

  const transferType = useMemo(() => ({
    all: {
      key: 'all_transfer',
      text: '全部故事',
    },
    anyone: {
      key: 'anyone_transfer',
      text: '任一故事',
    },
  }), [record]);

  const switchProject = useCallback((id: string) => {
    setCurrentProject(id);
    setDataSet(new DataSet({
      autoCreate: true,
      fields: [{
        name: 'issueTypeId',
        label: '选择工作项类型',
        required: true,
      }, {
        name: 'statusId',
        label: '指定状态',
        required: true,
      }, {
        name: 'type',
        required: true,
        defaultValue: transferType.all.key,
      }],
      events: {
        update: ({ name, record: current }: { name: string, record: Record }) => {
          if (name === 'issueTypeId') {
            current.init('statusId');
          }
        },
      },
    }));
  }, [transferType]);
  useEffect(() => {
    if (!currentProject && subProjects && subProjects?.length > 0) {
      switchProject(subProjects[0].projectId);
    }
  }, [currentProject, subProjects, switchProject]);
  const projectLinkages = useMemo(() => linkagesMap.get(currentProject as string) || [], [currentProject, linkagesMap]);
  const [dataSet, setDataSet] = useState<DataSet>();
  useEffect(() => {
    setLoading(true);
    statusTransformApi.getFeatureLinkage(record.get('id'), issueTypeId).then((res: IFeatureLinkageSetting[]) => {
      batchedUpdates(() => {
        const map = new Map();
        res.forEach((item) => {
          if (!map.has(item.projectId)) {
            map.set(item.projectId, []);
          }
          map.get(item.projectId).push({
            issueTypeId: item.issueTypeId,
            statusId: item.statusId,
            projectId: item.projectId,
            type: item.type || 'all_transfer',
          });
        });
        setLinkagesMap(map);
        setLoading(false);
      });
    }).catch(() => {
      setLoading(false);
    });
  }, [Field, record, issueTypeId]);
  const prepareData = useCallback(async () => {
    const hasValue = dataSet?.find((r) => r.get('issueTypeId'));
    if (hasValue && !await dataSet?.validate()) {
      return false;
    }
    const data: any = dataSet?.toData();
    linkagesMap.set(currentProject as string, hasValue ? data.map((item: any) => ({ projectId: currentProject, ...item })) : []);
    return true;
  }, [currentProject, dataSet, linkagesMap]);
  useEffect(() => {
    const handleOk = async () => {
      if (!await prepareData()) {
        return false;
      }
      const data = [...linkagesMap.values()].reduce((result, current) => result.concat(current), []);
      await statusTransformApi.updateFeatureLinkage(record.get('id'), data, issueTypeId);
      customCirculationDataSet.query(customCirculationDataSet.currentPage);
      return true;
    };
    if (modal) {
      modal.handleOk(handleOk);
    }
  }, [customCirculationDataSet, modal, record, fields, dataSet, linkagesMap, prepareData, issueTypeId]);

  const handleProjectChange = useCallback(async (id: string) => {
    if (!await prepareData()) {
      return;
    }
    switchProject(id);
  }, [prepareData, switchProject]);
  return (
    <div className={`${styles.featureLinkage}`}>
      <Loading loading={loading} />
      <div className={styles.tip}>{`当子项目的故事流转到指定状态时，关联的特性自动流转到${record.get('name')}状态。`}</div>
      <div style={{ display: 'flex', flex: 1, overflow: 'hidden' }}>
        <div style={{ flex: 1, overflow: 'hidden' }}>
          <ProjectList value={currentProject} onChange={handleProjectChange} />
        </div>
        <div style={{ flex: 2 }}>
          {currentProject && dataSet && (
            <ProjectConfig
              key={currentProject}
              projectId={currentProject}
              parentIssueTypeId={issueTypeId}
              dataSet={dataSet}
              linkages={projectLinkages}
              transferType={transferType}
            />
          )}
        </div>
      </div>
    </div>
  );
};

export default observer(FeatureLinkage);
