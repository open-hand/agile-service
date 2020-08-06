import { FieldType } from 'choerodon-ui/pro/lib/data-set/enum';

const SortTableDataSet = () => ({
  autoQuery: true,
  dataKey: 'content',
  paging: false,
  selection: undefined,
  fields: [
    { name: 'fieldName', type: 'string' as FieldType, label: '字段名称' },
    { name: 'defaultValue', type: 'string' as FieldType, label: '默认值' },
    { name: 'require', type: 'boolean' as FieldType, label: '是否必填' },
    { name: 'edit', type: 'boolean' as FieldType, label: '是否加入到编辑页' },
    { name: 'create', type: 'boolean' as FieldType, label: '是否加入到创建页' },
  ],
  transport: {
    read: {
      url: '/agile/v1/projects/1528/page_field/list?organizationId=7&pageCode=agile_issue_edit',
      method: 'get',
    },
  },
});
export default SortTableDataSet;
