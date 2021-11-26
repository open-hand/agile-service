import React from 'react';
import { FieldType } from 'choerodon-ui/pro/lib/data-set/enum';
import { C7NFormat } from '@choerodon/master';
import DataSet, { DataSetProps } from 'choerodon-ui/pro/lib/data-set/DataSet';
import { getIsOrganization } from '@/utils/common';

import { issueTypeApiConfig } from '@/api';

const sourceDataSet = new DataSet({
  fields: [
    {
      name: 'code',
      type: 'string' as FieldType,
    },
    {
      name: 'label',
      type: 'string' as FieldType,
    },
  ],
  data: [
    {
      code: 'system',
      label: '系统',
    },
    {
      code: 'organization',
      label: '组织',
    },
    ...(getIsOrganization() ? [] : [
      {
        code: 'project',
        label: '项目',
      },
    ]),
  ],
});

const IssueTypeDataSet = ({ isOrganization }: { isOrganization: boolean }): DataSetProps => {
  const getQueryFields = () => {
    const queryFields = [
      {
        name: 'name',
        type: 'string' as FieldType,
        label: (
          <C7NFormat
            intlPrefix="agile.issueType"
            id="name"
          />
        ),
      },
      {
        name: 'source',
        type: 'string' as FieldType,
        label: (
          <C7NFormat
            intlPrefix="boot"
            id="source"
          />
        ),
        textField: 'label',
        valueField: 'code',
        options: sourceDataSet,
      },
    ];
    if (!isOrganization) {
      queryFields.push({
        name: 'enabled',
        label: (
          <C7NFormat
            intlPrefix="agile.issueType"
            id="state"
          />
        ),
        type: 'string' as FieldType,
        textField: 'label',
        valueField: 'value',
        options: new DataSet({
          data: [
            { label: '启用', value: 'true' },
            { label: '停用', value: 'false' },
          ],
        }),
      });
    }
    return queryFields;
  };

  return ({
    selection: false,
    paging: true,
    autoQuery: true,
    queryFields: getQueryFields(),
    fields: [
      {
        name: 'name',
        type: 'string' as FieldType,
        label: (
          <span>
            <C7NFormat
              intlPrefix="agile.issueType"
              id="name"
            />
          </span>
        ),
      },
      {
        name: 'action',
      },
      {
        name: 'description',
        type: 'string' as FieldType,
        label: (
          <span>
            <C7NFormat
              intlPrefix="agile.common"
              id="description"
            />
          </span>
        ),
      },
      {
        name: 'typeCode',
        type: 'string' as FieldType,
        label: (
          <span>
            <C7NFormat
              intlPrefix="agile.issueType"
              id="norm.type"
            />
          </span>
        ),
      },
      {
        name: 'usage',
        type: 'string' as FieldType,
        label: '使用情况',
      },
      {
        label: (
          <span>
            <C7NFormat
              intlPrefix="boot"
              id="source"
            />
          </span>
        ),
        name: 'source',
        type: 'string' as FieldType,
      },
      {
        label: (
          <span>
            <C7NFormat
              intlPrefix="agile.issueType"
              id="state"
            />
          </span>
        ),
        name: 'enabled',
        type: 'boolean' as FieldType,
      },
      {
        label: '是否允许引用',
        name: 'referenced',
        type: 'boolean' as FieldType,
      },
    ],
    transport: {
      read: ({ params, data }: { params: { page: number, size: number }, data: any }) => {
        if (getIsOrganization()) {
          return issueTypeApiConfig.orgLoad({ params, data });
        }
        return issueTypeApiConfig.load({ params, data });
      },
    },
  });
};
export default IssueTypeDataSet;