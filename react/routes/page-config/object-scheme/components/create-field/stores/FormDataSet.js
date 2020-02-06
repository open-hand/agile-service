/* eslint-disable consistent-return */
/* eslint-disable default-case */

import moment from 'moment';


function getLookupConfig(code) {
  return {
    url: `/agile/v1/lookup_values/${code}`,
    method: 'get',
    transformResponse: (response) => {
      try {
        const data = JSON.parse(response);
        if (data && data.lookupValues) {
          return data.lookupValues;
        } else {
          return data;
        }
      } catch (error) {
        return response;
      }
    },
  };
}

export default ({
  formatMessage, type, store, schemeCode, id, isEdit,
}) => {
  const regex = /^[0-9a-zA-Z_]+$/;

  // eslint-disable-next-line consistent-return
  async function checkCode(value) {
    if (isEdit) return; 
    if (!value) {
      return '';
    } else if (!regex.test(value)) {
      return formatMessage({ id: 'field.code.rule' });
    } else {
      const prefix = type === 'project' ? 'pro_' : 'org_';
      try {
        // error:此接口一直返回false
        const data = await store.checkCode(`${prefix}${value}`, schemeCode);
        if (data) {
          return formatMessage({ id: 'field.code.exist' });
        }
      } catch (error) {
        return formatMessage({ id: 'network.error' });
      }
    }
  }
  // eslint-disable-next-line consistent-return
  async function checkName(value) {
    if (!value) {
      return '';
    } else {
      const data = await store.checkName(value, schemeCode);
      if (data) {
        return formatMessage({ id: 'field.name.exist' });
      }
    }
  }

  return {
    autoCreate: true,
    autoQuery: false,
    transport: {
      
    },
    fields: [
      {
        name: 'code',
        type: 'string',
        label: formatMessage({ id: 'code' }),
        required: !isEdit,
        maxLength: 10,
        validator: checkCode,
      },
      {
        name: 'name',
        type: 'string',
        label: formatMessage({ id: 'name' }),
        required: true,
        maxLength: 6,
        validator: checkName,
      },
      {
        name: 'fieldType',
        type: 'string',
        label: formatMessage({ id: 'field.type' }),
        required: !isEdit,
        lookupAxiosConfig: getLookupConfig('field_type'),
        valueField: 'valueCode',
        textField: 'name',
      },
      {
        name: 'context',
        type: 'string',
        label: formatMessage({ id: 'field.context' }),
        required: true,
        multiple: true,
        valueField: 'valueCode',
        textField: 'name',
        lookupAxiosConfig: getLookupConfig('object_scheme_field_context'),
      },
      {
        name: 'defaultValue',
        label: formatMessage({ id: 'field.default' }),
        required: false,
        valueField: 'id',
        textField: 'realName',
        dynamicProps: {
          type: ({ dataSet, record, name }) => {
            const fieldType = record.get('fieldType');
            switch (fieldType) {
              case 'time':
                return 'time';
              case 'datetime':
                return 'dateTime';
              case 'date':
                return 'date';
              case 'input':
                return 'string';
              case 'text':
                return 'string';
              case 'url':
                return 'string';
            }
          },
          defaultValue: ({ record }) => {
            const fieldType = record.get('fieldType');
            switch (fieldType) {
              case 'time':
                return moment('00:00:00', 'HH:mm:ss');
              case 'datetime':
                return moment('00:00:00', 'HH:mm:ss');
            }
          },
          lookupAxiosConfig: ({ record }) => {
            const fieldType = record.get('fieldType');
            if (fieldType === 'member') {
              return {
                url: `/base/v1/${type}s/${id}/users`,
                method: 'get',
                transformResponse: (response) => {
                  try {
                    const data = JSON.parse(response);
                    if (data && data.list) {
                      return data.list;
                    } else {
                      return data;
                    }
                  } catch (error) {
                    return response;
                  }
                },
              };
            }
          },
        },
      },
      {
        name: 'check',
        defaultValue: false,
        type: 'boolean',
      },
    ],
  };
};
