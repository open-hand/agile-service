import React, {
  useEffect, useState, useCallback, useRef, useMemo,
} from 'react';
import {
  DataSet, Col, Row,
} from 'choerodon-ui/pro';
import { observer } from 'mobx-react-lite';
import { unstable_batchedUpdates as batchedUpdates } from 'react-dom';
import {
  statusTransformApi, IFeatureLinkage,
} from '@/api';
import Loading from '@/components/Loading';
import useFields from '@/routes/Issue/components/BatchModal/useFields';
import useSubProjects from '@/hooks/data/useSubProjects';
import ProjectList from './feature-linkage/ProjectList';
import ProjectConfig from './feature-linkage/ProjectConfig';
import styles from './index.less';

export interface IFeatureLinkageSetting {
  id: string
  issueTypeId: string
  projectId: string
  statusId: string
}

interface IFieldK {
  key: number,
}

const FeatureLinkage = ({
  // @ts-ignore
  modal, record, issueTypeId, customCirculationDataSet,
}) => {
  const [loading, setLoading] = useState(false);
  const { data: subProjects } = useSubProjects({ onlySelectEnable: true });
  const [fields, Field] = useFields();
  const [currentProject, setCurrentProject] = useState<string | null>(null);
  const [linkages, setLinkages] = useState<IFeatureLinkageSetting[]>([]);
  const switchProject = useCallback((id: string) => {
    setCurrentProject(id);
    setDataSet(new DataSet({
      autoCreate: true,
      events: {
        // @ts-ignore
        update: ({ name }) => {
          // const [key, selectName] = name.split('-');
          // if (selectName === 'project') {
          //   dataSet.current?.set(`${key}-status`, undefined);
          // }
        },
      },
    }));
  }, []);
  useEffect(() => {
    if (!currentProject && subProjects && subProjects?.length > 0) {
      switchProject(subProjects[0].projectId);
    }
  }, [currentProject, subProjects, switchProject]);
  const projectLinkages = useMemo(() => linkages.filter((link) => link.projectId === currentProject), [currentProject, linkages]);
  const [dataSet, setDataSet] = useState<DataSet>();
  useEffect(() => {
    setLoading(true);
    statusTransformApi.getFeatureLinkage(record.get('id'), issueTypeId).then((res: IFeatureLinkageSetting[]) => {
      batchedUpdates(() => {
        setLinkages(res);
        // if (linkages?.length) {
        //   const initFields = Field.init(new Array(linkages.length).fill({}));
        //   initFields.forEach((item: { key: number }, i: number) => {
        //     addFieldLinkage(item.key, i);
        //   });
        //   linkages.forEach((item, i: number) => {
        //     const {
        //       projectId, statusId,
        //     } = item;
        //     const { key } = initFields[i];
        //       current?.set(`${key}-project`, projectId);
        //       current?.set(`${key}-status`, statusId);
        //   });
        // } else {
        //   const newKey = Field.add();
        //   addFieldLinkage(newKey, 0);
        // }
        setLoading(false);
      });
    }).catch(() => {
      setLoading(false);
    });
  }, [Field, record, issueTypeId]);

  useEffect(() => {
    const handleOk = async () => {
      if (await dataSet?.validate()) {
        const current = dataSet?.current;
        const updateData: IFeatureLinkage[] = [];
        fields.forEach((f: IFieldK) => {
          const { key } = f;
          updateData.push({
            projectId: current?.get(`${key}-project`),
            statusId: current?.get(`${key}-status`),
            issueTypeId: current?.get(`${key}-storyId`),
          });
        });
        await statusTransformApi.updateFeatureLinkage(record.get('id'), updateData);
        customCirculationDataSet.query(customCirculationDataSet.currentPage);
        return true;
      }
      return false;
    };
    if (modal) {
      modal.handleOk(handleOk);
    }
  }, [customCirculationDataSet, modal, record, fields, dataSet]);
  const handleProjectChange = useCallback(async (id: string) => {
    const data: any = dataSet?.current?.toData();
    // eslint-disable-next-line no-underscore-dangle
    delete data.__dirty;
    const hasValue = Object.keys(data).find((key) => !!data[key]);
    if (hasValue && !await dataSet?.validate()) {
      return;
    }
    switchProject(id);
  }, [dataSet, switchProject]);
  return (
    <div className={`${styles.linkage} ${styles.featureLinkage}`}>
      <Loading loading={loading} />
      <div className={styles.tip}>{`当子项目的故事全部流转到指定状态时，关联的特性自动流转到${record.get('name')}状态。`}</div>
      <Row>
        <Col span={8}>
          <ProjectList value={currentProject} onChange={handleProjectChange} />
        </Col>
        <Col span={16}>
          {currentProject && dataSet && (
            <ProjectConfig
              key={currentProject}
              projectId={currentProject}
              dataSet={dataSet}
              linkages={projectLinkages}
            />
          )}
        </Col>
      </Row>
    </div>
  );
};

export default observer(FeatureLinkage);
