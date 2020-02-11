const IssueTypeDataSet = (
  formatMessage, organizationId,
) => ({
  selection: false,
  paging: true,
  autoQuery: true,
  queryFields: [
    {
      name: 'name',
      type: 'string',
      label: formatMessage({ id: 'issueType.name' }),
    },
    {
      name: 'description',
      type: 'string',
      label: formatMessage({ id: 'issueType.des' }),
    },
  ],
  fields: [
    {
      name: 'name',
      type: 'string',
      label: formatMessage({ id: 'issueType.name' }),
    },
    {
      name: 'description',
      type: 'string',
      label: formatMessage({ id: 'issueType.des' }),
    },
  ],
  transport: {
    read: () => ({
      url: `/agile/v1/organizations/${organizationId}/issue_type/list`,
      method: 'post',
    }),
  },
});
export default IssueTypeDataSet;
