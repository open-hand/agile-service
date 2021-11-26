import React from 'react';
import { FieldType } from 'choerodon-ui/pro/lib/data-set/enum';
import { DataSetProps } from 'choerodon-ui/pro/lib/data-set/DataSet';
import { C7NFormat } from '@choerodon/master';

const SortTableDataSet = (): DataSetProps => ({
  autoQuery: false,
  // dataKey: 'content',
  paging: false,
  selection: undefined,
  fields: [
    {
      name: 'fieldName',
      type: 'string' as FieldType,
      label: <C7NFormat
        intlPrefix="agile.page"
        id="field.name"
      />,
    },
    {
      name: 'defaultValue',
      type: 'string' as FieldType,
      label: (<C7NFormat
        intlPrefix="agile.page"
        id="default"
      />),
    },
    {
      name: 'required',
      type: 'boolean' as FieldType,
      label: (<C7NFormat
        intlPrefix="agile.page"
        id="config.project.require"
      />),
    },
    {
      name: 'edited',
      type: 'boolean' as FieldType,
      label: (<C7NFormat
        intlPrefix="agile.page"
        id="filed.can.edit"
      />),
    },
    {
      name: 'created',
      type: 'boolean' as FieldType,
      label: (<C7NFormat
        intlPrefix="agile.page"
        id="filed.can.create"
      />),
    },
    { name: 'rank', type: 'string' as FieldType, label: '排序' },
  ],

});
export default SortTableDataSet;
