import moment from 'moment';
import { remove, intersectionBy } from 'lodash';
import Record from 'choerodon-ui/pro/lib/data-set/Record';
import { DataSet } from 'choerodon-ui/pro/lib';
import { RenderProps } from 'choerodon-ui/pro/lib/field/FormField';
import { DataSetProps } from 'choerodon-ui/pro/lib/data-set/DataSet';
import { FieldType } from 'choerodon-ui/pro/lib/data-set/enum';
// import jsonBig from 'json-bigint';
import { getOrganizationId } from '@/utils/common';
import { pageConfigApiConfig } from '@/api';
import { Store } from './useStore';

interface Props {
  formatMessage: any,
  type: string,
  schemeCode: string,
  store: Store,
  id: string,
  isEdit: boolean,
  oldRecord?: Record,
  defaultContext?: string[],
  localCheckCode?: (code: string) => Promise<boolean> | boolean,
  localCheckName?: (name: string) => Promise<boolean> | boolean,
}
function getLookupConfig(code: string, filterArr?: string[], type?: string, id?: string) {
  return {
    url: `/agile/v1/lookup_values/${code}`,
    method: 'get',
    params: type && { [`${type}Id`]: id },
    transformResponse: (response: any) => {
      try {
        const data = JSON.parse(response);
        if (data && data.lookupValues) {
          if (code === 'object_scheme_field_context') {
            const waitRemove = filterArr ? [...filterArr, 'global'] : ['global'];
            remove(data.lookupValues, (item: any) => waitRemove.some((w) => w === item.valueCode));
          }
          return data.lookupValues;
        }
        return data;
      } catch (error) {
        if (code === 'object_scheme_field_context') {
          const waitRemove = filterArr ? [...filterArr, 'global'] : ['global'];
          remove(response, (item: any) => waitRemove.some((w) => w === item.valueCode));
        }
        return response;
      }
    },
  };
}
const dateList = ['time', 'datetime', 'date'];

const FormDataSet = ({
  formatMessage, type, store, schemeCode, id, isEdit,
  oldRecord, localCheckCode, localCheckName, defaultContext,
}: Props): DataSetProps => {
  const regex = /^[0-9a-zA-Z_]+$/;
  async function checkCode(value: string): Promise<boolean | string | undefined> {
    if (isEdit) return true;
    if (!value) {
      return true;
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
    if (isEdit && value === oldRecord?.get('name')) return true;
    if (!value) {
      return true;
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

    return true;
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
    if (isEdit && name === 'context') {
      const contextValues = [...record?.get('context')];

      const currentFieldValue = [...record?.get('syncIssueType')];
      // 清除不存在的值
      if (currentFieldValue.length > 0) {
        const newFieldValue = contextValues.length > 0 ? currentFieldValue.filter((item) => contextValues.includes(item)) : undefined;
        if (!newFieldValue || newFieldValue.length < currentFieldValue.length) {
          record?.set('syncIssueType', newFieldValue);
        }
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
        // type: 'string' as FieldType,
        label: formatMessage({ id: 'field.context' }),
        required: true,
        multiple: true,
        valueField: 'id',
        textField: 'name',
        defaultValue: defaultContext || undefined,
        options: new DataSet({
          autoQuery: true,
          paging: false,
          transport: {
            read: ({
              ...pageConfigApiConfig.loadTypesOnCreate(),
              transformResponse: (res: any) => {
                const data = JSON.parse(res).filter((item: any) => item.enabled);
                if (isEdit) {
                  const issueTypeVOList = oldRecord?.get('issueTypeVOList')?.filter((item: any) => !item.enabled) || [];

                  issueTypeVOList.length > 0 && store.eternalContext.push(...issueTypeVOList.map((item:any) => item.id));
                  data.unshift(...issueTypeVOList);
                }
                console.log('data...', data);
                return data;
              },
            }),
          },
        }),
      },
      {
        name: 'defaultValue',
        label: formatMessage({ id: 'field.default' }),
        valueField: 'id',
        textField: 'realName',
        dynamicProps: {
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
      ...isEdit ? [{
        name: 'syncIssueType',
        label: formatMessage({ id: 'field.default.sync' }),
        valueField: 'id',
        textField: 'name',
        multiple: true,
        dynamicProps: {
          options: ({ record, name }: { record: Record, name: string }) => {
            const issueTypeVOList = record.get('context');
            if (issueTypeVOList && issueTypeVOList.length > 0) {
              const optionDataSet = new DataSet({
                autoCreate: false,
                autoQuery: false,
              });
              const records: Record[] = record.getField('context')?.options?.filter((item: Record) => item.get('enabled') && issueTypeVOList.includes(item.get('id'))) || [];
              const dataArr = records.map((item) => item.toData());
              optionDataSet.loadData(dataArr);
              return optionDataSet;
            }
            return undefined;
          },
        },
      }] : [],
    ],
    events: {
      update: handleUpdate,
    },
  };
};
export default FormDataSet;
