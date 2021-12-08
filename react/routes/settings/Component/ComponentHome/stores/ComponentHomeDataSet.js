import React from 'react';
import { DataSet } from 'choerodon-ui/pro';
import { C7NFormat } from '@choerodon/master';

function ComponentHomeDataSet({ id, formatMessage }) {
  const roleSet = new DataSet({
    data: [{
      meaning: '无',
      value: '无',
    }, {
      meaning: '模块负责人',
      value: '模块负责人',
    }],
  });
  return {
    autoQuery: true,
    selection: false,
    transport: {
      read: {
        url: `/agile/v1/projects/${id}/component/query_all?no_issue_test=true`,
        method: 'post',
        transformRequest: (data) => {
          const {
            defaultAssigneeRole, name, description, manager, params,
          } = data;
          const searchDTO = {
            searchArgs: { name, description, manager },
            advancedSearchArgs: {
              defaultAssigneeRole: defaultAssigneeRole ? [defaultAssigneeRole] : [],
              contents: '',
            },
            contents: params ? [params] : undefined,
          };
          return JSON.stringify(searchDTO);
        },
      },
    },
    fields: [
      {
        name: 'name',
        type: 'string',
        label: formatMessage({ id: 'agile.setting.component' }),

      },
      {
        name: 'issueCount',
        type: 'string',
        label: formatMessage({ id: 'agile.common.issue' }),
      },
      {
        name: 'manager',
        type: 'string',
        label: formatMessage({ id: 'agile.setting.responsible' }),
      },
      {
        name: 'description',
        type: 'string',
        label: formatMessage({ id: 'agile.setting.component.description' }),
      },
      {
        name: 'defaultAssigneeRole',
        type: 'string',
        label: formatMessage({ id: 'agile.setting.default.assignee' }),
      },
      {
        name: 'sequence',
        type: 'string',
        label: (
          <span>

            <C7NFormat
              intlPrefix="agile.setting"
              id="sequence"
            />

          </span>),
      },
    ],
    queryFields: [
      {
        name: 'name',
        type: 'string',
        label: formatMessage({ id: 'agile.setting.component' }),
      },
      {
        name: 'manager',
        type: 'string',
        label: formatMessage({ id: 'agile.setting.responsible' }),
      },
      {
        name: 'description',
        type: 'string',
        label: formatMessage({ id: 'agile.setting.component.description' }),
      },
      {
        name: 'defaultAssigneeRole',
        type: 'string',
        label: formatMessage({ id: 'agile.setting.default.assignee' }),
        options: roleSet,
      },
    ],
  };
}

export default ComponentHomeDataSet;
