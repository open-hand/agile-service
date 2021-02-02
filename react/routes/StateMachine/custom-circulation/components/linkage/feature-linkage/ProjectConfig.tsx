import useFields from '@/routes/Issue/components/BatchModal/useFields';
import React, { useCallback, useEffect } from 'react';
import DataSetField from 'choerodon-ui/pro/lib/data-set/Field';
import {
  Form, DataSet, Select, Button, Col, Row,
} from 'choerodon-ui/pro';
import { observer } from 'mobx-react-lite';
import { ButtonColor } from 'choerodon-ui/pro/lib/button/enum';
import SelectIssueType from '@/components/select/select-issue-type-pro';
import { IFeatureLinkageSetting } from '../FeatureLinkage';

interface IFieldK {
  key: number,
}
interface ProjectConfigProps {
  projectId: string
  linkages: IFeatureLinkageSetting[]
  dataSet: DataSet
}
const ProjectConfig: React.FC<ProjectConfigProps> = ({ projectId, dataSet, linkages }) => {
  const [fields, Field] = useFields();
  const removeField = useCallback((name) => {
    dataSet.fields?.delete(name);
    dataSet.current?.fields.delete(name);
  }, [dataSet]);

  const addField = useCallback((name, props) => {
    const field = new DataSetField({ ...props, name }, dataSet, dataSet.current);
    dataSet.current?.fields.set(name, field);
  }, [dataSet]);
  const removeLinkage = useCallback((key) => {
    removeField(`${key}-type`);
    removeField(`${key}-status`);
  }, [removeField]);
  const addFieldLinkage = useCallback((key) => {
    addField(`${key}-status`, { required: true });
    addField(`${key}-type`, { required: true });
  }, [addField]);
  useEffect(() => {
    if (fields.length > 0) {
      return;
    }
    if (linkages.length > 0) {
      const initFields = Field.init(new Array(linkages.length).fill({}));
      initFields.forEach((item: { key: number }, i: number) => {
        addFieldLinkage(item.key);
      });
      linkages.forEach((item, i: number) => {
        const { issueTypeId, statusId } = item;
        const { key } = initFields[i];
        dataSet.current?.set(`${key}-type`, issueTypeId);
        dataSet.current?.set(`${key}-status`, statusId);
      });
    } else {
      const newKey = Field.add();
      addFieldLinkage(newKey);
    }
  }, [Field, addFieldLinkage, dataSet, fields.length, linkages]);
  return (
    <Form dataSet={dataSet}>
      {
        fields.map(((f: IFieldK, i: number) => {
          const { key } = f;
          return (
            <div>
              <Row
                key={`${key}-type`}
                gutter={20}
                style={{
                  marginBottom: 27,
                }}
              >
                <Col span={20}>
                  <SelectIssueType
                    config={{
                      projectId,
                      applyType: 'agile',
                      typeCode: 'story',
                    }}
                    name={`${key}-type`}
                    placeholder="选择问题类型"
                    clearButton={false}
                  />
                </Col>
                <Col span={2}>
                  <Button
                    onClick={() => {
                      Field.remove(key);
                      removeLinkage(key);
                    }}
                    icon="delete"
                  />
                </Col>
              </Row>
              <Row key={`${key}-status`} gutter={20}>
                <Col span={20}>
                  {/* <SelectStatus
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
                    /> */}
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
            addFieldLinkage(newKey);
          }}
          icon="add"
          color={'blue' as ButtonColor}
        >
          添加联动
        </Button>
      </div>
    </Form>
  );
};
export default observer(ProjectConfig);
