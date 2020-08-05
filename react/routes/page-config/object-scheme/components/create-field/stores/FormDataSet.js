/* eslint-disable consistent-return */
/* eslint-disable default-case */

import moment from 'moment';
import { remove } from 'lodash';

function getLookupConfig(code) {
  return {
    url: `/agile/v1/lookup_values/${code}`,
    method: 'get',
    transformResponse: (response) => {
      try {
        const data = JSON.parse(response);
        if (data && data.lookupValues) {
          if (code === 'object_scheme_field_context') {
            remove(data.lookupValues, (item) => item.valueCode === 'global');
          }
          return data.lookupValues;
        }
        return data;
      } catch (error) {
        return response;
      }
    },
  };
}
const dateList = ['time', 'datetime', 'date'];

export default ({
  formatMessage, type, store, schemeCode, id, isEdit, oldRecord, userOptionDataSet,
}) => {
  const regex = /^[0-9a-zA-Z_]+$/;
  // eslint-disable-next-line consistent-return
  async function checkCode(value) {
    if (isEdit) return;
    if (!value) {
      return '';
    } if (!regex.test(value)) {
      return formatMessage({ id: 'field.code.rule' });
    }
    const prefix = type === 'project' ? 'pro_' : 'org_';
    try {
      const data = await store.checkCode(`${prefix}${value}`, schemeCode);
      if (data) {
        return formatMessage({ id: 'field.code.exist' });
      }
    } catch (error) {
      return formatMessage({ id: 'network.error' });
    }
  }
  // eslint-disable-next-line consistent-return
  async function checkName(value) {
    if (isEdit && value === oldRecord.get('name')) return;
    if (!value) {
      return '';
    }
    const data = await store.checkName(value, schemeCode);
    if (data) {
      return formatMessage({ id: 'field.name.exist' });
    }
  }

  function handleUpdate({ record, name, value }) {
    if (name === 'check' && value) {
      const fieldType = record.get('fieldType');
      if (dateList.indexOf(fieldType) !== -1) {
        record.set('defaultValue', moment());
      }
    } else if (value && name === 'fieldType') {
      record.set('defaultValue', null);
      record.set('check', false);
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
        maxLength: isEdit ? 15 : 10,
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
      // 用于提交更新使用
      {
        name: 'updateFieldOptions',
        type: 'object',
        required: false,
        // ignore: 'always',
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
        valueField: 'id',
        textField: 'realName',
        dynamicProps: {
          options: ({ record }) => {
            const fieldType = record.get('fieldType');
            if (fieldType === 'member') {
              return userOptionDataSet;
            }
          },
          max: ({ record }) => {
            const fieldType = record.get('fieldType');
            if (fieldType === 'number') {
              return Number.MAX_SAFE_INTEGER;
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
    events: {
      update: handleUpdate,
    },
  };
};
