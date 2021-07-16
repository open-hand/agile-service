import React from 'react';
import {
  Form, DataSet, Button, Col, Row, Icon,
} from 'choerodon-ui/pro';
import { observer } from 'mobx-react-lite';
import SelectLinkType from '@/components/select/pro/select-link-type';
import SelectIssuesInLink from '@/components/select/select-issues-in-link';

interface IssueLinkProps {
  projectId?: string
  dataSet: DataSet
}
const IssueLink: React.FC<IssueLinkProps> = ({
  projectId, dataSet,
}) => (
  <Form dataSet={dataSet}>
    {dataSet.map((record) => (
      <Row gutter={20} type="flex" align="middle">
        <Col span={11}>
          <SelectLinkType
            style={{ width: '100%' }}
            record={record}
            projectId={projectId}
            name="linkType"
            clearButton={false}
          />
        </Col>
        <Col span={11}>
          <SelectIssuesInLink
            style={{ width: '100%' }}
            record={record}
            projectId={projectId}
            name="issueIds"
            clearButton={false}
            disabled={!record.get('linkType')}
            key={record.get('linkType')}
            multiple
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
        添加链接
      </Button>
    </div>
  </Form>
);
export default observer(IssueLink);
