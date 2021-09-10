import moment from 'moment';
import {
  orderBy, remove, set,
} from 'lodash';
import Record from 'choerodon-ui/pro/lib/data-set/Record';
import { DataSet } from 'choerodon-ui/pro';
import { RenderProps } from 'choerodon-ui/pro/lib/field/FormField';
import { DataSetProps } from 'choerodon-ui/pro/lib/data-set/DataSet';
import { FieldType } from 'choerodon-ui/pro/lib/data-set/enum';
import { MAX_LENGTH_FIELD_CODE, MAX_LENGTH_FIELD_NAME } from '@/constants/MAX_LENGTH';
import { MAX_NUMBER_VALUE, MAX_NUMBER_STEP } from '@/constants/MAX_VALUE';
import { Store } from './useStore';
import { IFieldType } from '@/common/types';

interface Props {
  formatMessage: any,
  type: string,
  schemeCode: string,
  store: Store,
  id: string,
  isEdit: boolean,
  oldRecord?: Record,
  defaultContext?: string[],
  contextOptionsDataSet: DataSet
  localCheckCode?: (code: string) => Promise<boolean> | boolean,
  localCheckName?: (name: string) => Promise<boolean> | boolean,
}
interface IFieldTypeLookupDataItem {
  description: string
  name: string
  objectVersionNumber: 1
  typeCode: 'field_type'
  valueCode: IFieldType
}
function attachRankForFieldType(data: Array<IFieldTypeLookupDataItem>): Array<IFieldTypeLookupDataItem & { rank: number }> {
  const rankMap: { [P in IFieldType]?: number } = {
    multiple: 12,
    single: 12,
    radio: 9,
    checkbox: 9,
    member: 8,
    date: 7,
    multiMember: 6,
  };
  return data.map((item) => ({ ...item, rank: rankMap[item.valueCode] || 10 }));
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
          if (code === 'field_type') {
            set(data, 'lookupValues', orderBy(attachRankForFieldType(data.lookupValues), (a) => a.rank, 'desc'));
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
  } as const;
}
const dateList = ['time', 'datetime', 'date'];

const FormDataSet = ({
  formatMessage, type, store, schemeCode, id, isEdit, contextOptionsDataSet,
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
        maxLength: isEdit ? MAX_LENGTH_FIELD_CODE + 5 : MAX_LENGTH_FIELD_CODE,
        validator: checkCode,
      },
      {
        name: 'name',
        type: 'string' as FieldType,
        label: formatMessage({ id: 'name' }),
        required: true,
        maxLength: MAX_LENGTH_FIELD_NAME,
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
        valueField: 'id',
        textField: 'name',
        options: contextOptionsDataSet,
      },
      {
        name: 'defaultValue',
        label: formatMessage({ id: 'field.default' }),
        dynamicProps: {
          max: ({ record }: { record: Record }) => {
            const fieldType = record.get('fieldType');
            if (fieldType === 'number') {
              return MAX_NUMBER_VALUE;
            }
            return undefined;
          },
          step: ({ record }: { record: Record }) => {
            const fieldType = record.get('fieldType');

            if (fieldType === 'number') {
              const enableFloat = record.get('check');
              return enableFloat ? MAX_NUMBER_STEP : 1;
            }
            return undefined;
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
