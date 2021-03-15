import React from 'react';
import {
  Button, Modal, Table, Tooltip,
} from 'choerodon-ui/pro';
import { RenderProps } from 'choerodon-ui/pro/lib/field/FormField';
import UserTag from '@/components/tag/user-tag';
import renderStatus from '@/components/column-renderer/status';
import renderPriority from '@/components/column-renderer/priority';
import { useReleaseDetailContext } from '../../stores';

const { Column } = Table;
function BugTable() {
  const { bugTableDataSet, isInProgram } = useReleaseDetailContext();
  function handleDelete() {
    Modal.confirm({
      title: '是否删除？',
      children: '确定删除该问题项与当前版本及应用版本库的关联关系？',
      // okText: '确定',
      // okProps: { disabled: true },
    });
  }
  function renderAction({}:RenderProps) {
    return <Button icon="delete_forever" onClick={handleDelete} />;
  }
  function renderSummary({ value }:RenderProps) {
    return (
      <Tooltip title={value} placement="topLeft">
        <span className="c7n-agile-table-cell-click">{value}</span>
      </Tooltip>
    );
  }
  function renderEpicOrFeature({ record, name: fieldName }:RenderProps) {
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
    <Table dataSet={bugTableDataSet}>
      <Column name="summary" lock={'left' as any} width={210} renderer={renderSummary} />
      <Column name="issueNum" width={120} tooltip={'overflow' as any} className="c7n-agile-table-cell" />
      <Column name="status" renderer={renderStatus} />
      <Column name="priority" renderer={renderPriority} />
      <Column name="influenceVersion" />
      <Column name="assigner" className="c7n-agile-table-cell" renderer={({ value }) => (value ? <UserTag data={value} /> : '')} />
      <Column name="action" lock={'right' as any} renderer={renderAction} width={60} />

    </Table>
  );
}
export default BugTable;
