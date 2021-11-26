import React from 'react';
import { DataSet } from 'choerodon-ui/pro';
import { C7NFormat } from '@choerodon/master';

function ComponentHomeDataSet({ id, intl }) {
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
        label: (
          <span>
            <C7NFormat
              intlPrefix="agile.setting"
              id="component"
            />
          </span>),
      },
      {
        name: 'issueCount',
        type: 'string',
        label: (
          <span>
            <C7NFormat
              intlPrefix="agile.common"
              id="issue"
            />
          </span>),
      },
      {
        name: 'manager',
        type: 'string',
        label: (
          <span>
            <C7NFormat
              intlPrefix="agile.setting"
              id="responsible"
            />

          </span>),
      },
      {
        name: 'description',
        type: 'string',
        label: (
          <span>

            <C7NFormat
              intlPrefix="agile.setting"
              id="component.description"
            />

          </span>),
      },
      {
        name: 'defaultAssigneeRole',
        type: 'string',
        label: '默认经办人',
      },
      {
        name: 'sequence',
        type: 'string',
        label: '模块顺序',
      },
    ],
    queryFields: [
      {
        name: 'name',
        type: 'string',
        label: <C7NFormat
          intlPrefix="agile.setting"
          id="component"
        />,
      },
      {
        name: 'manager',
        type: 'string',
        label: <C7NFormat
          intlPrefix="agile.setting"
          id="responsible"
        />,
      },
      {
        name: 'description',
        type: 'string',
        label: <C7NFormat
          intlPrefix="agile.setting"
          id="component.description"
        />,
      },
      {
        name: 'defaultAssigneeRole',
        type: 'string',
        label: '默认经办人',
        options: roleSet,
      },
    ],
  };
}

export default ComponentHomeDataSet;
