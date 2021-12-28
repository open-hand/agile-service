import React, { useEffect } from 'react';
import {
  Form, DataSet, Button, Col, Row,
  Icon, SelectBox,
} from 'choerodon-ui/pro';

import { observer } from 'mobx-react-lite';
import SelectIssueType from '@/components/select/pro/select-issue-type';
import SelectStatus from '@/components/select/select-status';
import { statusTransformApi } from '@/api';
import { IFeatureLinkageSetting } from './index';
import Styles from './ProjectConfig.less';

interface IFieldK {
  key: number,
}
interface ProjectConfigProps {
  projectId: string
  parentIssueTypeId: string
  linkages: IFeatureLinkageSetting[]
  dataSet: DataSet
  transferType: { anyone: { key: string, text: string }, all: { key: string, text: string } },
}

const { Option } = SelectBox;
const ProjectConfig: React.FC<ProjectConfigProps> = ({
  projectId, dataSet, linkages, parentIssueTypeId, transferType,
}) => {
  useEffect(() => {
    if (linkages.length > 0) {
      dataSet.loadData(linkages);
    }
  }, [dataSet, linkages]);
  const selectedIssueTypeIds = dataSet.map((record) => record.get('issueTypeId'));
  return (
    <Form className={Styles.configForm}>
      {dataSet.map((record) => {
        console.log('type===', record.get('type'));
        return (
          <Row gutter={20} type="flex" align="middle">
            <Col span={22}>
              <span className={Styles.checkLabel}>验证范围：</span>
              <SelectBox
                // 1.5.0的UI中的SelectBox不支持通过record={record}绑定ds,暂时先用value控制值
                value={record.get('type')}
                className={Styles.checkRequire}
                onChange={(value) => record.set('type', value)}
              >
                <Option value={transferType.anyone.key}>{transferType.anyone.text}</Option>
                <Option value={transferType.all.key}>{transferType.all.text}</Option>
              </SelectBox>
            </Col>
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
        )
      })}
      <div>
        <Button
          style={{ marginTop: -4 }}
          onClick={() => {
            dataSet.create();
            console.log(dataSet.toData());
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
