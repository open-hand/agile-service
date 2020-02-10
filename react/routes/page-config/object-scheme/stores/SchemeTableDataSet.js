

export default ({
  projectId, formatMessage, organizationId, schemeCode, type,
}) => ({
  selection: false,
  paging: false,
  autoQuery: true,
  dataKey: 'content',
  queryFields: [
    {
      name: 'name',
      type: 'string',
      label: formatMessage({ id: 'field' }),
    },
  ],
  fields: [
    {
      name: 'name',
      type: 'string',
      label: formatMessage({ id: 'field' }),
    },
    {
      name: 'contextName',
      type: 'string',
      label: formatMessage({ id: 'field.range' }),
    },
    {
      name: 'fieldTypeName',
      type: 'string',
      label: formatMessage({ id: 'field.type' }),
    },
    {
      name: 'required',
      type: 'boolean',
      label: formatMessage({ id: 'field.required' }),
    },
  ],
  transport: {
    read: () => ({
      url: `/agile/v1/${type}s/${projectId}/object_scheme_field/list?schemeCode=${schemeCode}&organizationId=${organizationId}`,
      method: 'get',
    }),
    destroy: ({ data: [data] }) => ({
      url: `/agile/v1/${type}s/${projectId}/object_scheme_field/${data.id}?organizationId=${organizationId}`,
      method: 'delete',
    }),
  },
});
