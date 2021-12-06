// @ts-nocheck
import React from 'react';
import {
  get, find, intersection,
} from 'lodash';
import { CheckBox, Tooltip } from 'choerodon-ui/pro';
import TypeTag from '@/components/TypeTag';
import { systemColumnsMap, getCustomColumn } from './baseColumns';
import './index.less';
import useFormatMessage from '@/hooks/useFormatMessage';

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
  const summaryColumn = systemColumnsMap.get('summary')!;
  const columnMap = new Map([['summary', { ...summaryColumn, render: ({ rowData }) => summaryColumn.render(rowData, get, { onClick: () => onSummaryClick(rowData) }) }]]);
  systemColumnsMap.forEach((column, key) => {
    if (!columnMap.has(key)) {
      columnMap.set(key, { ...column, render: column.render ? ({ rowData }) => column.render(rowData) : undefined });
    }
  });
  return columnMap;
};
const IntlField: React.FC<{ children: React.ReactElement, column: any }> = ({ children, column }) => {
  const formatMessage = useFormatMessage();
  const name = column.titleKey ? formatMessage({ id: column.titleKey }) : column.title; // , defaultMessage: column.title
  return React.isValidElement(name) ? name : <Tooltip title={name}>{name}</Tooltip>;
};
export function getTableColumns({
  listLayoutColumns, fields, onSummaryClick, handleColumnResize,
}) {
  const res = [];
  const columnsMap = getColumnsMap({ onSummaryClick });
  const getCustom = (field) => {
    const column = getCustomColumn(field);
    return { ...column, render: ({ rowData }) => column.render(rowData) };
  };
  listLayoutColumns.forEach((layoutColumn) => {
    const { columnCode: code, width } = layoutColumn;
    const field = find(fields, { code });
    if (field) {
      // 系统字段和自定义字段处理
      const column = columnsMap.has(code) ? columnsMap.get(code) : getCustom(field);
      res.push({
        ...column,
        title: <IntlField column={column} />,
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
