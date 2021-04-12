import React from 'react';
import {
  Button, Modal, Table, Tooltip,
} from 'choerodon-ui/pro';
import { RenderProps } from 'choerodon-ui/pro/lib/field/FormField';
import UserTag from '@/components/tag/user-tag';
import Record from 'choerodon-ui/pro/lib/data-set/Record';
import { publishVersionApi, versionApi } from '@/api';
import renderStatus from '@/components/column-renderer/status';
import { useReleaseDetailContext } from '../../stores';

const { Column } = Table;
function StoryTable() {
  const { storyTableDataSet, isInProgram, store } = useReleaseDetailContext();

  function handleDelete(record: Record) {
    Modal.confirm({
      title: '是否删除关联',
      children: `确定要删除问题${record.get('issueNum')}与版本的关联？删除后，将移除故事和当前版本下应用版本的关联关系。`,
      onOk: () => publishVersionApi.deleteLinkIssueId(record.get('issueId'), store.current?.id!).then(() => {
        storyTableDataSet.query();
      }),
    });
  }
  function renderAction({ record }: RenderProps) {
    return <Button icon="delete_forever" onClick={() => handleDelete(record!)} />;
  }
  function renderSummary({ value }: RenderProps) {
    return (
      <Tooltip title={value} placement="topLeft">
        <span className="c7n-agile-table-cell-click">{value}</span>
      </Tooltip>
    );
  }
  function renderEpicOrFeature({ record, name: fieldName }: RenderProps) {
    const color = fieldName === 'epic' ? record?.get('epicColor') : record?.get('featureColor');
    const name = fieldName === 'epic' ? record?.get('epicName') : record?.get('featureName');
    return name ? (
      <Tooltip title={name}>
        <span style={{
          width: '100%',
          color,
          borderWidth: '1px',
          borderStyle: 'solid',
          borderColor: color,
          borderRadius: '2px',
          fontSize: '13px',
          lineHeight: '20px',
          padding: '0 4px',
          display: 'inline-block',
          overflow: 'hidden',
          textOverflow: 'ellipsis',
          whiteSpace: 'nowrap',
        }}
        >
          {name}
        </span>
      </Tooltip>
    ) : null;
  }

  return (
    <Table dataSet={storyTableDataSet}>
      <Column
        name="summary"
        lock={'left' as any}
        width={210}
        onCell={({ record }) => ({
          onClick: () => {
            store.selectIssue(record.get('issueId'));
          },
        })}
        renderer={renderSummary}
      />
      <Column name="issueNum" width={120} tooltip={'overflow' as any} className="c7n-agile-table-cell" />
      <Column name="status" renderer={({ record }) => (record?.get('statusVO') ? renderStatus({ record }) : undefined)} />
      <Column
        name="appVersions"
        className="c7n-agile-table-cell"
        tooltip={'overflow' as any}
        width={120}
        renderer={({ value }) => (value ? value.map((i: any) => `${i.artifactId}/${i.versionAlias || i.version}`).join('、') : undefined)}
      />
      <Column name={isInProgram ? 'featureVO' : 'epic'} renderer={renderEpicOrFeature} className="c7n-agile-table-cell" />
      <Column
        name="assigneeId"
        className="c7n-agile-table-cell"
        renderer={({ value, record }) => (value ? (
          <UserTag data={{
            imageUrl: record?.get('assigneeImageUrl'),
            loginName: record?.get('assigneeLoginName'),
            realName: record?.get('assigneeRealName'),
            tooltip: record?.get('assigneeName'),
          }}
          />
        ) : '')}
      />
      <Column name="action" lock={'right' as any} renderer={renderAction} width={60} />

    </Table>
  );
}
export default StoryTable;
