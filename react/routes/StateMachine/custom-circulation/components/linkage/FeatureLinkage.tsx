import React, {
  useMemo, useEffect, useState, useCallback, useRef,
} from 'react';
import {
  Form, DataSet, Select, Button, Col, Row,
} from 'choerodon-ui/pro';
import DataSetField from 'choerodon-ui/pro/lib/data-set/Field';
import { observer } from 'mobx-react-lite';
import { includes } from 'lodash';
import { unstable_batchedUpdates as batchedUpdates } from 'react-dom';
import SelectStatus from '@/components/select/select-status';
import {
  statusTransformApi, commonApi, issueTypeApi, IFeatureLinkage,
} from '@/api';
import Loading from '@/components/Loading';
import useFields from '@/routes/Issue/components/BatchModal/useFields';

import { ButtonColor } from 'choerodon-ui/pro/lib/button/enum';
import styles from './index.less';

interface IFeatureLinkageSetting {
  id: string
  issueTypeId: string
  projectId: string
  statusId: string
}

interface IFieldK {
  key: number,
}

interface ISubProject {
  projectId: string,
  projName: string
}

const { Option } = Select;

const FeatureLinkage = ({
// @ts-ignore
  modal, record, selectedType, customCirculationDataSet,
}) => {
  const [loading, setLoading] = useState(false);
  const [fields, Field] = useFields();
  const [subProjects, setSubProjects] = useState<ISubProject[]>([]);
  const modalDataSetRef = useRef<DataSet>();

  useEffect(() => {
    commonApi.getSubProjects(true).then((projects: ISubProject[]) => {
      setSubProjects(projects);
    });
  }, []);

  const modalDataSet = useMemo(() => new DataSet({
    autoCreate: true,
    events: {
      // @ts-ignore
      update: ({ dataSet, name }) => {
        const [key, selectName] = name.split('-');
        if (selectName === 'project') {
          dataSet.current?.set(`${key}-status`, undefined);
        }
      },
    },
  }), []);

  modalDataSetRef.current = modalDataSet;

  const removeField = useCallback((name) => {
    modalDataSetRef.current?.fields?.delete(name);
    modalDataSetRef.current?.current?.fields.delete(name);
  }, []);

  const addField = useCallback((name, props) => {
    const field = new DataSetField({ ...props, name }, modalDataSet, modalDataSet.current);
    modalDataSet?.current?.fields.set(name, field);
  }, [modalDataSet]);

  const addFieldLinkage = useCallback((key, i) => {
    addField(`${key}-project`, {
      required: true,
    });
    addField(`${key}-status`, {
      required: true,
    });
    addField(`${key}-storyId`, {});
  }, [addField]);

  useEffect(() => {
    const { current } = modalDataSet;
    setLoading(true);
    statusTransformApi.getFeatureLinkage(record.get('id'), 'feature').then((linkages: IFeatureLinkageSetting[]) => {
      batchedUpdates(() => {
        if (linkages?.length) {
          const initFields = Field.init(new Array(linkages.length).fill({}));
          initFields.forEach((item: { key: number }, i: number) => {
            addFieldLinkage(item.key, i);
          });
          linkages.forEach((item, i: number) => {
            const {
              projectId, statusId,
            } = item;
            const { key } = initFields[i];
              current?.set(`${key}-project`, projectId);
              current?.set(`${key}-status`, statusId);
          });
        } else {
          const newKey = Field.add();
          addFieldLinkage(newKey, 0);
        }
        setLoading(false);
      });
    }).catch(() => {
      setLoading(false);
    });
  }, [Field, addFieldLinkage, modalDataSet, record, selectedType]);

  useEffect(() => {
    const handleOk = async () => {
      if (await modalDataSet.validate()) {
        const current = modalDataSet?.current;
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
  }, [customCirculationDataSet, modalDataSet, modal, record, selectedType, fields]);

  const existProjects: string[] = [];
  fields.forEach((field: IFieldK) => {
    const { key } = field;
    const keyProject = modalDataSet?.current?.get(`${key}-project`);
    const project = subProjects.find((item: ISubProject) => item.projectId === keyProject);
    if (project) {
      existProjects.push(project.projectId);
    }
  });

  return (
    <div className={`${styles.linkage} ${styles.featureLinkage}`}>
      <Loading loading={loading} />
      <div className={styles.tip}>{`当子项目的故事全部流转到指定状态时，关联的特性自动流转到${record.get('name')}状态。`}</div>
      <Form dataSet={modalDataSet}>
        {
          fields.map(((f: IFieldK, i: number) => {
            const { key } = f;
            return (
              <div>
                <Row
                  key={`${key}-project`}
                  gutter={20}
                  style={{
                    marginBottom: 27,
                  }}
                >
                  <Col span={20}>
                    <Select
                      name={`${key}-project`}
                      placeholder="选择项目"
                      clearButton={false}
                    >
                      {
                        subProjects.filter((project: ISubProject) => (
                          modalDataSet?.current?.get(`${key}-project`) === project.projectId
                        ) || !includes(existProjects, project.projectId)).map((project) => (
                          <Option key={project.projectId} value={project.projectId}>
                            {project.projName}
                          </Option>
                        ))
                      }
                    </Select>
                  </Col>
                  <Col span={2}>
                    <Button
                      onClick={() => {
                        batchedUpdates(() => {
                          // @ts-ignore
                          Field.remove(key);
                          removeField(`${key}-project`);
                          removeField(`${key}-status`);
                        });
                      }}
                      icon="delete"
                    />
                  </Col>
                </Row>
                <Row key={`${key}-status`} gutter={20}>
                  <Col span={20}>
                    {
                      !modalDataSet?.current?.get(`${key}-project`) ? (
                        <Select name={`${key}-status`} placeholder="指定状态" disabled />
                      ) : (
                        <SelectStatus
                          name={`${key}-status`}
                          placeholder="指定状态"
                          clearButton={false}
                          key={`${modalDataSet?.current?.get(`${key}-project`)}`}
                          request={async () => {
                            const projectIssueTypes = await issueTypeApi.loadAllWithStateMachineId('agile', modalDataSet?.current?.get(`${key}-project`));
                            const issueTypeId = (projectIssueTypes || []).find((item) => item.typeCode === 'story')?.id;
                            if (issueTypeId) {
                              modalDataSet.current?.set(`${key}-storyId`, issueTypeId);
                              return statusTransformApi.getFeatureLinkageStatus({ issueTypeId, projectId: modalDataSet?.current?.get(`${key}-project`), parentIssueStatusSetting: record.get('id') });
                            }
                            return Promise.resolve([]);
                          }}
                        />
                      )
                    }
                  </Col>
                </Row>
              </div>
            );
          }))
        }
        <div>
          <Button
            // @ts-ignore
            onClick={() => {
              const newKey = Field.add();
              addFieldLinkage(newKey, fields.length);
            }}
            icon="add"
            color={'blue' as ButtonColor}
          >
            添加联动
          </Button>
        </div>
      </Form>
    </div>
  );
};

export default observer(FeatureLinkage);
