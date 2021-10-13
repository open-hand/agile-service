// @ts-nocheck
import React from 'react';
import {
  get, find, intersection,
} from 'lodash';
import { CheckBox, Tooltip } from 'choerodon-ui/pro';
import TypeTag from '@/components/TypeTag';
import { systemColumnsMap, getCustomColumn } from './baseColumns';
import './index.less';

export const checkBoxColumn = ({
  checkValues, data, handleCheckChange, handleCheckAllChange, checkValuesRef,
}) => {
  const keys = data.map((i) => i.issueId);
  const pageCheckedKeys = intersection(keys, checkValues);
  const checked = pageCheckedKeys.length > 0;
  const allChecked = pageCheckedKeys.length === keys.length;
  const indeterminate = checked && !allChecked;
  return ({
    title: (
      <CheckBox
        indeterminate={indeterminate}
        checked={checked}
        onChange={handleCheckAllChange}
      />
    ),
    dataIndex: 'issueId',
    key: 'issueId',
    width: 40,
    fixed: true,
    render: ({ rowData, dataIndex, rowIndex }) => (
      <CheckBox
        key={rowIndex}
        value={rowData.issueId}
        checked={checkValuesRef.current.includes(rowData.issueId)}
        onChange={(value) => handleCheckChange(value, rowData.issueId)}
      />
    ),
  });
};
export const expandColumn = {
  title: '',
  dataIndex: 'issueId',
  key: 'issueId',
  className: 'expand-column',
  width: 10,
  fixed: true,
  treeCol: true,
  render: () => null,
};

const getColumnsMap = ({ onSummaryClick }) => {
  const columnMap = new Map([['summary', {
    title: <Tooltip title="概要">概要</Tooltip>,
    dataIndex: 'summary',
    width: 400,
    fixed: true,
    sortable: true,
    render: ({ rowData }) => (
      <>
        <TypeTag data={get(rowData, 'issueTypeVO')} style={{ marginRight: 5, marginTop: -2 }} />
        <Tooltip mouseEnterDelay={0.5} placement="topLeft" title={`工作项概要： ${get(rowData, 'summary')}`}>
          <span role="none" className="c7n-agile-table-cell-click" onClick={() => onSummaryClick(rowData)}>
            {get(rowData, 'summary')}
          </span>
        </Tooltip>
      </>
    ),
  }]]);
  systemColumnsMap.forEach((column, key) => {
    if (!columnMap.has(key)) {
      columnMap.set(key, { ...column, render: column.render ? ({ rowData }) => column.render(rowData) : undefined });
    }
  });
  return columnMap;
};

export function getTableColumns({
  listLayoutColumns, fields, onSummaryClick, handleColumnResize,
}) {
  const res = [];
  const columnsMap = getColumnsMap({ onSummaryClick });
  listLayoutColumns.forEach((layoutColumn) => {
    const { columnCode: code, width } = layoutColumn;
    const field = find(fields, { code });
    if (field) {
      // 系统字段和自定义字段处理
      const column = columnsMap.has(code) ? columnsMap.get(code) : getCustomColumn(field);
      res.push({
        ...column,
        code,
        display: layoutColumn.display,
        resizable: true,
        onResize: handleColumnResize,
        width: width && width > 0 ? width : column.width,
      });
    }
  });
  return res;
}
