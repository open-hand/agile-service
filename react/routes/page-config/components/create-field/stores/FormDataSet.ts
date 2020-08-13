import moment from 'moment';
import { remove } from 'lodash';
import Record from 'choerodon-ui/pro/lib/data-set/Record';
import { DataSet } from 'choerodon-ui/pro/lib';
import { RenderProps } from 'choerodon-ui/pro/lib/field/FormField';
import { DataSetProps } from 'choerodon-ui/pro/lib/data-set/DataSet';
import { FieldType } from 'choerodon-ui/pro/lib/data-set/enum';
import { Store } from './useStore';

interface Props {
  formatMessage: any,
  type: string,
  schemeCode: string,
  store: Store,
  id: string,
  isEdit: boolean,
  oldRecord?: Record,
  userOptionDataSet: DataSet,
  localCheckCode?: (code: string) => Promise<boolean> | boolean,
  localCheckName?: (name: string) => Promise<boolean> | boolean,
}
function getLookupConfig(code: string) {
  return {
    url: `/agile/v1/lookup_values/${code}`,
    method: 'get',
    transformResponse: (response: any) => {
      try {
        const data = JSON.parse(response);
        if (data && data.lookupValues) {
          if (code === 'object_scheme_field_context') {
            remove(data.lookupValues, (item: any) => item.valueCode === 'global');
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

const FormDataSet = ({
  formatMessage, type, store, schemeCode, id, isEdit,
  oldRecord, userOptionDataSet, localCheckCode, localCheckName,
}: Props): DataSetProps => {
  const regex = /^[0-9a-zA-Z_]+$/;
  async function checkCode(value: string): Promise<boolean | string | undefined> {
    if (isEdit) return false;
    if (!value) {
      return false;
    } if (!regex.test(value)) {
      return formatMessage({ id: 'field.code.rule' });
    }
    const prefix = type === 'project' ? 'pro_' : 'org_';
    try {
      let data: boolean = await store.checkCode(`${prefix}${value}`, schemeCode);
      if (!data && localCheckCode) {
        data = await localCheckCode(`${prefix}${value}`);
      }
      if (data) {
        return formatMessage({ id: 'field.code.exist' });
      }
    } catch (error) {
      return formatMessage({ id: 'network.error' });
    }
    return undefined;
  }
  async function checkName(value: string): Promise<boolean | string | undefined> {
    if (isEdit && value === oldRecord?.get('name')) return false;
    if (!value) {
      return false;
    }
    try {
      let data: boolean = await store.checkName(value, schemeCode);
      if (!data && localCheckName) {
        data = await localCheckName(value);
      }
      if (data) {
        return formatMessage({ id: 'field.name.exist' });
      }
    } catch (error) {
      return formatMessage({ id: 'network.error' });
    }

    return undefined;
  }

  function handleUpdate({ record, name, value }: RenderProps) {
    if (name === 'check' && value) {
      const fieldType = record?.get('fieldType');
      if (dateList.indexOf(fieldType) !== -1) {
        record?.set('defaultValue', moment());
      }
    } else if (value && name === 'fieldType') {
      record?.set('defaultValue', null);
      record?.set('check', false);
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
        type: 'string' as FieldType,
        label: formatMessage({ id: 'code' }),
        required: !isEdit,
        maxLength: isEdit ? 15 : 10,
        validator: checkCode,
      },
      {
        name: 'name',
        type: 'string' as FieldType,
        label: formatMessage({ id: 'name' }),
        required: true,
        maxLength: 6,
        validator: checkName,
      },
      {
        name: 'fieldType',
        type: 'string' as FieldType,
        label: formatMessage({ id: 'field.type' }),
        required: !isEdit,
        lookupAxiosConfig: getLookupConfig('field_type'),
        valueField: 'valueCode',
        textField: 'name',
      },
      // 用于提交更新使用
      {
        name: 'updateFieldOptions',
        type: 'object' as FieldType,
        required: false,
        // ignore: 'always',
      },
      {
        name: 'context',
        type: 'string' as FieldType,
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
          options: ({ record }: { record: Record }) => {
            const fieldType = record.get('fieldType');
            if (fieldType === 'member') {
              return userOptionDataSet;
            }
            return null;
          },
          max: ({ record }: { record: Record }) => {
            const fieldType = record.get('fieldType');
            if (fieldType === 'number') {
              return Number.MAX_SAFE_INTEGER;
            }
            return null;
          },
        },
      },
      {
        name: 'check',
        defaultValue: false,
        type: 'boolean' as FieldType,
      },
    ],
    events: {
      update: handleUpdate,
    },
  };
};
export default FormDataSet;
