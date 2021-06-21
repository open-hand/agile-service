import React, { useEffect } from 'react';
import {
  Form, DataSet, Button, Col, Row,
} from 'choerodon-ui/pro';
import { Icon } from 'choerodon-ui';
import { observer } from 'mobx-react-lite';
import { ButtonColor } from 'choerodon-ui/pro/lib/button/enum';
import SelectIssueType from '@/components/select/pro/select-issue-type';
import SelectStatus from '@/components/select/select-status';
import { statusTransformApi } from '@/api';
import { IFeatureLinkageSetting } from './index';

interface IFieldK {
  key: number,
}
interface ProjectConfigProps {
  projectId: string
  parentIssueTypeId: string
  linkages: IFeatureLinkageSetting[]
  dataSet: DataSet
}
const ProjectConfig: React.FC<ProjectConfigProps> = ({
  projectId, dataSet, linkages, parentIssueTypeId,
}) => {
  useEffect(() => {
    if (linkages.length > 0) {
      dataSet.loadData(linkages);
    }
  }, [dataSet, linkages]);
  const selectedIssueTypeIds = dataSet.map((record) => record.get('issueTypeId'));
  return (
    <Form dataSet={dataSet} style={{ padding: '10px 20px' }}>
      {dataSet.map((record) => (
        <Row gutter={20} type="flex" align="middle">
          <Col span={11}>
            <SelectIssueType
              record={record}
              config={{
                projectId,
                applyType: 'agile',
                typeCode: 'story',
              }}
              name="issueTypeId"
              clearButton={false}
              optionsFilter={(re) => !selectedIssueTypeIds.includes(re.get('id')) || record.get('issueTypeId') === re.get('id')}
            />
          </Col>
          <Col span={11}>
            <SelectStatus
              record={record}
              name="statusId"
              clearButton={false}
              disabled={!record.get('issueTypeId')}
              key={record.get('issueTypeId')}
              request={async () => {
                const issueTypeId = record.get('issueTypeId');
                if (issueTypeId) {
                  return statusTransformApi.getFeatureLinkageStatus({ issueTypeId, projectId }, parentIssueTypeId);
                }
                return Promise.resolve([]);
              }}
            />
          </Col>
          <Col span={2}>
            <Icon
              onClick={() => {
                dataSet.remove(record);
              }}
              type="delete_sweep-o"
              style={{
                color: 'var(--primary-color)',
                cursor: 'pointer',
              }}
            />
          </Col>
        </Row>
      ))}
      <div>
        <Button
          style={{ marginTop: -4 }}
          onClick={() => {
            dataSet.create();
          }}
          icon="add"
        >
          添加联动
        </Button>
      </div>
    </Form>
  );
};
export default observer(ProjectConfig);
