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
  <Form dataSet={dataSet} columns={23}>
    {dataSet.map((record) => ([
      <SelectLinkType
        style={{ width: '100%' }}
        record={record}
        projectId={projectId}
        name="linkType"
        clearButton={false}
        // @ts-ignore
        colSpan={11}
      />,
      <SelectIssuesInLink
        style={{ width: '100%' }}
        record={record}
        projectId={projectId}
        name="issueIds"
        clearButton={false}
        disabled={!record.get('linkType')}
        key={record.get('linkType')}
        multiple
        // @ts-ignore
        colSpan={11}
      />,
      <span>
        <Icon
          onClick={() => {
            dataSet.remove(record);
          }}
          type="delete_sweep-o"
          style={{
            color: 'var(--primary-color)',
            cursor: 'pointer',
            lineHeight: '0.48rem',
          }}
        />
      </span>,
    ]))}
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
