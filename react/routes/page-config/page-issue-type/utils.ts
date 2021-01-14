import { IFieldOptionProps } from '@/api';
import Record from 'choerodon-ui/pro/lib/data-set/Record';
import moment from 'moment';
import { toJS } from 'mobx';
import { User } from '@/common/types';

const disabledEditDefaultFields = ['featureType', 'issueType', 'status', 'priority', 'creationDate', 'lastUpdateDate', 'timeTrace', 'belongToBacklog', 'urgent', 'progressFeedback', 'description'];
const orgDisabledEditDefaultFields = [...disabledEditDefaultFields, 'component', 'label', 'influenceVersion', 'fixVersion', 'epic', 'sprint', 'pi', 'subProject'];
const fieldTextValueConfig = {
  epic: { optionKey: 'issueId', textKey: 'epicName' },
  influenceVersion: { optionKey: 'versionId', textKey: 'name' },
  fixVersion: { optionKey: 'versionId', textKey: 'name' },
  component: { optionKey: 'componentId', textKey: 'name' },
  label: { optionKey: 'labelId', textKey: 'labelName' },
  sprint: { optionKey: 'sprintId', textKey: 'sprintName' },
  backlogType: { optionKey: 'id', textKey: 'name' },
  backlogClassification: { optionKey: 'id', textKey: 'name' },
};
function transformDefaultValue({
  fieldType, defaultValue, defaultValueObj, fieldOptions, optionKey: propsOptionKey = 'id', textKey: propsTextKey = 'value', fieldCode,
}: { fieldType: string, defaultValue: any, defaultValueObj?: any, fieldOptions?: Array<IFieldOptionProps> | Array<User> | null, optionKey?: 'tempKey' | 'id' | string, textKey?: 'value' | string, fieldCode?: string }) {
  if (!defaultValue && !defaultValueObj) {
    return defaultValue;
  }
  const { optionKey = propsOptionKey, textKey = propsTextKey } = fieldTextValueConfig[fieldCode as keyof typeof fieldTextValueConfig] || {};
  switch (fieldType) {
    case 'datetime':
      return moment(defaultValue, 'YYYY-MM-DD HH:mm:ss').format('YYYY-MM-DD HH:mm:ss');
    case 'time':
      return moment(defaultValue, 'YYYY-MM-DD HH:mm:ss').format('HH:mm:ss');
    case 'date':
      return moment(defaultValue, 'YYYY-MM-DD HH:mm:ss').format('YYYY-MM-DD');
    case 'multiple':
    case 'checkbox':
    case 'single':
    case 'radio': {
      const valueArr = String(defaultValue).split(',');
      return (fieldOptions as Array<IFieldOptionProps> | undefined)?.filter((option) => valueArr.some((v) => v === option[optionKey as keyof typeof option])).map((item) => item[textKey as keyof typeof item]).join(',') || defaultValue;
    }
    case 'member': {
      const { realName } = defaultValueObj || {};
      return realName || defaultValue;
    }
    case 'multiMember': {
      return String(Array.isArray(fieldOptions) ? (fieldOptions as User[]).map((item) => item.realName) : (defaultValueObj?.realName || ''));
    }
    default:
      return defaultValue;
  }
}
function beforeSubmitTransform(item: Record, optionKey = 'id') {
  let fieldOptions = item.get('fieldOptions') as Array<any> | undefined;
  const defaultValue = toJS(item.get('defaultValue'));
  const fieldType = item.get('fieldType');
  if (fieldOptions && !defaultValue) {
    fieldOptions = fieldOptions.map((option) => ({ ...option, isDefault: false }));
  } else if (fieldOptions && ['radio', 'single', 'checkbox', 'multiple'].includes(fieldType)) {
    const searchDefaultArr = Array.isArray(defaultValue) ? defaultValue : [defaultValue];
    fieldOptions = fieldOptions.map((option) => {
      if (searchDefaultArr.includes(option[optionKey])) {
        return ({ ...option, isDefault: true });
      }
      return ({ ...option, isDefault: false });
    });
  }
  return {
    defaultValue: typeof (defaultValue) === 'undefined' || defaultValue === null ? '' : String(defaultValue),
    fieldOptions,
    extraConfig: item.get('extraConfig'),
  };
}

export {
  transformDefaultValue, beforeSubmitTransform, disabledEditDefaultFields, fieldTextValueConfig, orgDisabledEditDefaultFields,
};
