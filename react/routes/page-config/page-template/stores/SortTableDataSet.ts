import { FieldType } from 'choerodon-ui/pro/lib/data-set/enum';
import { DataSetProps } from 'choerodon-ui/pro/lib/data-set/DataSet';

const SortTableDataSet = (): DataSetProps => ({
  autoQuery: false,
  // dataKey: 'content',
  paging: false,
  // selection: '',
  fields: [
    { name: 'fieldName', type: 'string' as FieldType, label: '字段名称' },
    { name: 'defaultValue', type: 'string' as FieldType, label: '默认值' },
    { name: 'required', type: 'boolean' as FieldType, label: '必填' },
    { name: 'edited', type: 'boolean' as FieldType, label: '加入到编辑页' },
    { name: 'created', type: 'boolean' as FieldType, label: '加入到创建页' },
    { name: 'rank', type: 'string' as FieldType, label: '排序' },
  ],

});
export default SortTableDataSet;
