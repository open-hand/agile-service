import React, {
  useMemo, useEffect, useState, useCallback, useRef,
} from 'react';
import {
  Form, DataSet, Select, Button, Col, Row,
} from 'choerodon-ui/pro';
import DataSetField from 'choerodon-ui/pro/lib/data-set/Field';
import { observer } from 'mobx-react-lite';
import { find, includes } from 'lodash';
import { unstable_batchedUpdates as batchedUpdates } from 'react-dom';
import { useIssueTypes } from '@/hooks';
import { IIssueType } from '@/common/types';
import SelectStatus from '@/components/select/select-status';
import { statusTransformApi, commonApi } from '@/api';
import { FieldType } from 'choerodon-ui/pro/lib/data-set/enum';
import Loading from '@/components/Loading';
import SelectTeam from '@/components/select/select-team';
import useFields from '@/routes/Issue/components/BatchModal/useFields';

import { ButtonColor } from 'choerodon-ui/pro/lib/button/enum';
import styles from './index.less';

interface IFeatureLinkageSettingItem {
  projectId: string
  statusId: string
}

interface IFeatureLinkageSetting {
  id: string
  issueTypeId: string
  linkages: IFeatureLinkageSettingItem[]
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
  const [issueTypes] = useIssueTypes();
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
    // events: {
    //   update: () => {

    //   },
    // }
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
  }, [addField]);

  useEffect(() => {
    const { current } = modalDataSet;
    if (selectedType) { // 有selectedTypeCode的时候再请求，防止请求两边，浪费一次没有意义的请求
      setLoading(true);
      statusTransformApi.getLinkage(selectedType, record.get('id')).then((res: IFeatureLinkageSetting) => {
        setLoading(false);
        const { linkages } = res;
        batchedUpdates(() => {
          const initFields = Field.init(new Array(linkages.length).fill({}));
          initFields.forEach((item: { key: number }, i: number) => {
            addFieldLinkage(item.key, i);
          });
          linkages.forEach((item: IFeatureLinkageSettingItem, i: number) => {
            const {
              projectId, statusId,
            } = item;
            const { key } = initFields[i];
            current?.set(`${key}-code`, projectId);
            current?.set(`${key}-operation`, statusId);
          });
          setLoading(false);
        });
      }).catch(() => {
        setLoading(false);
      });
    }
  }, [Field, addFieldLinkage, modalDataSet, record, selectedType]);

  useEffect(() => {
    const handleOk = async () => {
      console.log('validate, data');
      console.log(await modalDataSet.validate(), modalDataSet.current);
      if (await modalDataSet.validate()) {
        const current = modalDataSet?.current;
        // @ts-ignore
        const updateData: any[] = [];
        fields.forEach((f: IFieldK) => {
          const { key } = f;
          updateData.push({
            projectId: current?.get(`${key}-project`),
            statusId: current?.get(`${key}-status`),
          });
        });
        console.log('updateData：');
        console.log(updateData);
        await statusTransformApi.updateLinkage(selectedType, record.get('id'), record.get('objectVersionNumber'), updateData);
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
      <div className={styles.tip}>我是描述。</div>
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
                  <Col span={22}>
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
                  <Col span={22}>
                    <SelectStatus
                      disabled={!modalDataSet?.current?.get(`${key}-project`)}
                      name={`${key}-status`}
                      placeholder="指定状态"
                      issueTypeCode="story"
                      projectId={modalDataSet?.current?.get(`${key}-project`)}
                      applyType="agile"
                      clearButton={false}
                    />
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
