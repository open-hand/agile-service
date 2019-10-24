import transform from '../utils';

export default ({
  intl, projectId, organizationId, intlPrefix,
}) => ({
  primaryKey: 'issueId',
  autoQuery: false,
  selection: false,
  // selection: 'single',
  transport: {
    read: {
      url: `/agile/v1/projects/${projectId}/issues/include_sub?organizationId=${organizationId}`,
      method: 'post',
      transformRequest: data => JSON.stringify(transform(data)),
    },
  },
  fields: [
    { name: 'issueId', type: 'string', label: '概要' },
    { name: 'issueTypeId', type: 'object', label: '问题类型' },
    { name: 'issueNum', type: 'string', label: '问题编号' },
    { name: 'priorityId', type: 'string', label: '优先级' },    
    { name: 'statusId', type: 'object', label: '状态' },
    { name: 'assigneeId', type: 'string', label: '经办人' },
    { name: 'reporterId', type: 'string', label: '报告人' },
    { name: 'label', type: 'string', label: '标签' },
    { name: 'component', type: 'string', label: '模块' },
    { name: 'storyPoints', type: 'string', label: '故事点' },
    { name: 'version', type: 'string', label: '版本' },
    { name: 'epic', type: 'string', label: '史诗' },
    { name: 'lastUpdateDate', type: 'string', label: '最后更新时间' },
    { name: 'issueSprintVOS', type: 'array', label: '冲刺' },    
  ],
  queryFields: [
    { name: 'issueTypeId', type: 'array', label: '问题类型' },
    // { name: 'service', type: 'string', label: service },
    // { name: 'description', type: 'string', label: description },      
  ],
});
