import React from 'react';
import { Tooltip } from 'choerodon-ui/pro';
import TableDropMenu from '@/components/table-drop-menu';
import { pageConfigApi } from '@/api';
import ToggleFieldValue from '../components/toggle-field-value';
import openLinkage from '../components/setting-linkage/Linkage';

import openPageRoleConfigModal from './components/role-config';

interface LinkPagePermission {
  include: string[]
  ignoreOtherInclude?: boolean
}
interface LinkPagePermissionWithKey extends LinkPagePermission {
  key: string
}
/**
 * include  通过所有include 即显示
 * ignoreOtherInclude 是否忽略其他include  @default false
 */
const LinkPagePermissionMap = new Map<string, LinkPagePermission>([
  ['fieldType', { include: ['radio', 'single', 'multiple', 'checkbox'] }],
  ['createdLevel', { include: ['project'] }],
  ['fieldCode', { include: ['priority', 'fixVersion', 'component', 'influenceVersion'], ignoreOtherInclude: true }],

]);
/**
 * 检查级联权限
 * @param data
 * @returns
 */
function checkPermissionLinkage(data: any) {
  const keyFilters = Object.keys(data).filter((key) => LinkPagePermissionMap.has(key)).map((key) => ({ key, ...LinkPagePermissionMap.get(key) })) as LinkPagePermissionWithKey[];
  if (keyFilters.length === 0) {
    return true;
  }
  const res = keyFilters.filter((filter) => !filter?.ignoreOtherInclude).every((filter) => filter?.include?.includes(data[filter.key]));
  return res || keyFilters.filter((filter) => filter?.ignoreOtherInclude).some((filter) => filter?.include?.includes(data[filter.key]));
}

const getColumns = ({ issueTypeId, loadData, disabled }: { issueTypeId: string, loadData: () => void, disabled:boolean }) => ([

  {
    title: '字段名称',
    dataIndex: 'fieldName',
    key: 'fieldName',
    width: 120,
    fixed: true,
    render: ({ rowData }: any) => <Tooltip title={rowData.get('fieldName')}>{rowData.get('fieldName')}</Tooltip>,
    // return <DraggableOrgEditItem data={new Record(rowData)} />;

  },
  {
    title: '默认值',
    dataIndex: 'defaultValue',
    key: 'defaultValue',
    width: 200,
    render: ({ rowData }: any) => <div style={{ display: 'inline-flex', alignItems: 'center', width: '99%' }}><ToggleFieldValue data={rowData} disabled={disabled} /></div>,
  },
  {
    title: '角色权限说明',
    dataIndex: 'roleInfo',
    key: 'roleInfo',
    flexGrow: 1,
    // width: 120,
    render: ({ rowData }: any) => {
      const permissionList = rowData.get('permissionList') || [];
      const readPermission = permissionList.find((i: any) => i.scope === 'read');
      return (readPermission ? (
        <Tooltip title={`【${[...readPermission.roleList, ...readPermission.userList].map((i) => i.name).join('、')}】可见`}>
          <span>{`【${[...readPermission.roleList, ...readPermission.userList].map((i) => i.name).join('、')}】可见`}</span>
        </Tooltip>
      ) : '');
    },
  },
  {
    title: '级联说明',
    dataIndex: 'cascadeInfo',
    key: 'cascadeInfo',
    flexGrow: 1,
    // width: 120,
    render: ({ rowData }: any) => (rowData.get('fieldCascadeRuleDesList')?.length ? (
      <Tooltip title={`设置级联${rowData.get('fieldCascadeRuleDesList').map((item: { cascadeFieldName: string }) => item.cascadeFieldName).join('、')}`}>
        <span>{`设置级联${rowData.get('fieldCascadeRuleDesList').map((item: { cascadeFieldName: string }) => item.cascadeFieldName).join('、')}`}</span>
      </Tooltip>
    ) : ''),
  },
  {
    title: '操作',
    dataIndex: 'action',
    key: 'action',
    width: 80,
    render: ({ rowData }: any) => (disabled ? null : (
      <TableDropMenu
        menuData={[{
          text: '权限配置',
          action: async () => {
            const res = await pageConfigApi.loadFieldPermission(rowData.get('fieldId'), issueTypeId);
            openPageRoleConfigModal({
              fields: [{ id: rowData.get('fieldId'), code: rowData.get('fieldCode') }], data: res, issueTypeId, onOk: loadData,
            });
          },
          display: rowData.get('allowedEditPermission'), // checkPermissionRole({ createdLevel: rowData.get('createdLevel') }),
        }, {
          text: '设置级联规则',
          action: () => {
            openLinkage({
              issueTypeId,
              field: {
                id: rowData.get('fieldId'),
                name: rowData.get('fieldName'),
                fieldCode: rowData.get('fieldCode'),
                system: rowData.get('createdLevel') === 'system',
              },
              onOk: loadData,
            });
          },
          display: checkPermissionLinkage(rowData.toData()),
        }]}
        defaultMenuIcon="settings-o"
        defaultButtonProps={{ size: 'default' as any }}
        showText={false}
      />
    )),
  },
]);

export { getColumns };
