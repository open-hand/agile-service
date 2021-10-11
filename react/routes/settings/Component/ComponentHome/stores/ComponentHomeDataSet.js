import { DataSet } from 'choerodon-ui/pro';

export default ({ id, intl }) => {
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
        label: '模块',
      },
      {
        name: 'issueCount',
        type: 'string',
        label: '工作项',
      },
      {
        name: 'manager',
        type: 'string',
        label: '负责人',
      },
      {
        name: 'description',
        type: 'string',
        label: '模块描述',
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
        label: '模块',
      },
      {
        name: 'manager',
        type: 'string',
        label: '负责人',
      },
      {
        name: 'description',
        type: 'string',
        label: '模块描述',
      },
      {
        name: 'defaultAssigneeRole',
        type: 'string',
        label: '默认经办人',
        options: roleSet,
      },
    ],
  };
};
