import { IFieldOption } from '@/api';
import moment from 'moment';
import { IFieldType } from '@/common/types';
import { getMenuType } from '@/utils/common';
import Record from 'choerodon-ui/pro/lib/data-set/Record';

const singleList = ['radio', 'single'];
const multipleList = ['checkbox', 'multiple'];

interface IUpdateFieldPostData {
  code: string
  context: string[]
  defaultValue: string
  extraConfig: boolean
  fieldOptions?: Array<IFieldOption & { isDefault: boolean }>
  fieldType: IFieldType
  name: string
  issueTypeIds: string[]
  objectVersionNumber?: number
  schemeCode: 'agile_issue' | string
}
function beforeSubmitProcessData(record: Record, { fieldOptions: propsFieldOptions, schemeCode = 'agile_issue' }: { fieldOptions?: Array<any>, schemeCode?: string } = {}): IUpdateFieldPostData {
  const originDefaultValue = record.get('defaultValue');
  const fieldOptions: IFieldOption & { [propsName: string]: any }[] | undefined = propsFieldOptions || record.get('fieldOptions');
  const objectVersionNumber = record.get('objectVersionNumber');
  const obj = {
    fieldOptions: null as unknown as any,
    fieldType: record.get('fieldType'),
    defaultValue: String(originDefaultValue || ''),
  };

  if (singleList.indexOf(obj.fieldType) !== -1) {
    obj.fieldOptions = fieldOptions?.map((o: any) => {
      if (obj.defaultValue
        && (o.id === obj.defaultValue || o.code === obj.defaultValue
          || o.tempKey === obj.defaultValue)) {
        return { ...o, isDefault: true };
      }
      return { ...o, isDefault: false };
    });
  } else if (multipleList.indexOf(obj.fieldType) !== -1) {
    obj.fieldOptions = fieldOptions?.map((o: any) => {
      if (Array.isArray(originDefaultValue) && originDefaultValue.some((v) => v === o.id || v === o.tempKey || v === o.code)) {
        return { ...o, isDefault: true };
      }
      return { ...o, isDefault: false };
    });
    // if (obj.defaultValue && Array.isArray(obj.defaultValue)) {
    //   obj.defaultValue = obj.defaultValue.join(',');
    // }
  }
  const data = record.toData() as any;
  const dateList = ['date', 'datetime', 'time'];
  const prefix = getMenuType() === 'project' ? 'pro_' : 'org_';
  const { name, check } = data;
  const { issueTypeVOList, context: originContext } = data;
  const eternalDisabledOptions = issueTypeVOList?.filter((item: any) => !item.enabled).map((item:any) => item.id);
  console.log('eternalDisabledOptions', eternalDisabledOptions);
  const context = eternalDisabledOptions && eternalDisabledOptions.length > 0 ? originContext
    .filter((item:any) => !eternalDisabledOptions.includes(item)) : originContext;
  const transformTime = {} as { defaultValue: string };
  const dateFormat = ['YYYY-MM-DD', 'YYYY-MM-DD HH:mm:ss', 'HH:mm:ss'];
  const dateIndex = dateList.indexOf(data.fieldType);
  if (dateIndex !== -1 && obj.defaultValue !== '') {
    const dateFormatVal = moment(obj.defaultValue);
    transformTime.defaultValue = dateFormatVal.isValid() ? dateFormatVal.format(dateFormat[1]) : moment(obj.defaultValue, dateFormat).format(dateFormat[1]);
  }
  const postData = {
    context: context || issueTypeVOList.map((t: any) => t.id),
    issueTypeIds: context || issueTypeVOList.map((t: any) => t.id),
    code: `${prefix}${data.code}`,
    name,
    ...obj,
    ...transformTime,
    schemeCode,
    extraConfig: check,
    objectVersionNumber,
  };
  return postData;
}
export default beforeSubmitProcessData;
