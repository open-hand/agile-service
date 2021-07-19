import Record from 'choerodon-ui/pro/lib/data-set/Record';
import React from 'react';
import {
  Button, Modal, Spin, message, Select, Tooltip, PerformanceTable, Icon, Dropdown, CheckBox,
} from 'choerodon-ui/pro';
import TableDropMenu from '@/components/table-drop-menu';
import ToggleFieldValue from '../components/toggle-field-value';

const columns = [

  {
    title: '字段名称',
    dataIndex: 'fieldName',
    key: 'fieldName',
    width: 120,
    fixed: true,
    render: ({ rowData }: any) => rowData.get('fieldName'),
    // return <DraggableOrgEditItem data={new Record(rowData)} />;

  },
  {
    title: '默认值',
    dataIndex: 'defaultValue',
    key: 'defaultValue',
    width: 200,
    render: ({ rowData }: any) => <ToggleFieldValue data={rowData} />,
  },
  {
    title: '角色权限说明',
    dataIndex: 'roleInfo',
    key: 'roleInfo',
    flexGrow: 1,
    // width: 120,
  },
  {
    title: '级联说明',
    dataIndex: 'cascadeInfo',
    key: 'cascadeInfo',
    flexGrow: 1,
    // width: 120,
  },
  {
    title: '操作',
    dataIndex: 'action',
    key: 'action',
    width: 80,
    render: () => <TableDropMenu menuData={[{ text: '权限配置' }, { text: '设置级联规则' }]} defaultMenuIcon="settings-o" showText={false} />,
  },
];

export { columns };
