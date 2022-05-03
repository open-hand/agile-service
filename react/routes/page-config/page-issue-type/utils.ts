import Record from 'choerodon-ui/pro/lib/data-set/Record';
import moment from 'moment';
import { isEmpty } from 'lodash';
import { toJS } from 'mobx';
import { IFieldOptionProps } from '@/api';
import { User } from '@/common/types';
import { FORMAT_FIELDS, MINUTE } from '@/constants/DATE_FORMAT';

const disabledEditDefaultFields = ['featureType', 'issueType', 'status', 'priority', 'creationDate', 'lastUpdateDate', 'timeTrace', 'belongToBacklog', 'urgent', 'progressFeedback', 'description', 'environment', 'subProject', 'created_user', 'last_updated_user', 'email', 'tag', 'riskCategory', 'riskInfluence', 'riskProbability', 'riskProximity', 'copingStrategy'];
const orgDisabledEditDefaultFields = [...disabledEditDefaultFields, 'component', 'label', 'influenceVersion', 'fixVersion', 'epic', 'sprint', 'pi', 'subProject', 'backlogClassification', 'backlogType', 'programVersion'];
const fieldTextValueConfig = {
  epic: { optionKey: 'issueId', textKey: 'epicName' },
  influenceVersion: { optionKey: 'versionId', textKey: 'name' },
  fixVersion: { optionKey: 'versionId', textKey: 'name' },
  component: { optionKey: 'componentId', textKey: 'name' },
  label: { optionKey: 'labelId', textKey: 'labelName' },
  sprint: { optionKey: 'sprintId', textKey: 'sprintName' },
  backlogType: { optionKey: 'id', textKey: 'name' },
  backlogClassification: { optionKey: 'id', textKey: 'name' },
  pi: { optionKey: 'id', textKey: 'piName' },
  programVersion: { optionKey: 'id', textKey: 'name' },
  subProject: { optionKey: 'projectId', textKey: 'projName' },

};
function transformDefaultValue({
  fieldType, defaultValue, defaultValueObj, fieldOptions, optionKey: propsOptionKey = 'id', textKey: propsTextKey = 'value', fieldCode, extraConfig,
}: { fieldType: string, defaultValue: any, extraConfig?: boolean, defaultValueObj?: any, fieldOptions?: Array<IFieldOptionProps> | Array<User> | null, optionKey?: 'tempKey' | 'id' | string, textKey?: 'value' | string, fieldCode?: string }) {
  if (!defaultValue && !defaultValueObj) {
    return defaultValue || '';
  }
  const { optionKey = propsOptionKey, textKey = propsTextKey } = fieldTextValueConfig[fieldCode as keyof typeof fieldTextValueConfig] || {};
  switch (fieldType) {
    case 'datetime':
      return extraConfig ? '当前时间' : moment(defaultValue, 'YYYY-MM-DD HH:mm:ss').format(fieldCode && FORMAT_FIELDS.includes(fieldCode) ? MINUTE : 'YYYY-MM-DD HH:mm:ss');
    case 'time':
      return extraConfig ? '当前时间' : moment(defaultValue, ['YYYY-MM-DD HH:mm:ss', 'HH:mm:ss']).format('HH:mm:ss');
    case 'date':
      return extraConfig ? '当前时间' : moment(defaultValue, 'YYYY-MM-DD HH:mm:ss').format('YYYY-MM-DD');
    case 'multiple':
    case 'checkbox':
    case 'single':
    case 'radio': {
      const valueArr = String(defaultValue).split(',');
      const selectOptions = (fieldOptions as Array<IFieldOptionProps> | undefined)?.
        filter((option) => valueArr.some((v) => v === option[optionKey as keyof typeof option])).
        map((item) => item[textKey as keyof typeof item]) || [];
      return selectOptions.length > 0 ? selectOptions.join(',') : '';
    }
    case 'member': {
      const { realName } = Array.isArray(toJS(defaultValueObj)) ? defaultValueObj[0] : defaultValueObj || {};
      return realName || defaultValue;
    }
    case 'multiMember': {
      const memberDefaultValue = Array.isArray(defaultValue) ? defaultValue : String(defaultValue).split(',');
      return String(Array.isArray(toJS(fieldOptions)) && !isEmpty(memberDefaultValue) ? (fieldOptions as User[]).filter((item) => memberDefaultValue.some((d) => d === item.id)).map((item) => item.realName) : (defaultValueObj?.realName || ''));
    }
    default:
      return defaultValue;
  }
}
function beforeSubmitTransform(item: Record, optionKey = 'id') {
  let fieldOptions = item.get('fieldOptions') as Array<any> | undefined;
  let defaultValue = toJS(item.get('defaultValue'));
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
  } else if (fieldOptions && ['member', 'multiMember'].includes(fieldType)) {
    fieldOptions = undefined;
  }
  if (['datetime', 'time', 'date'].includes(fieldType)) {
    defaultValue = moment(defaultValue, ['YYYY-MM-DD HH:mm:ss', 'HH:mm:ss']).isValid() ? moment(defaultValue, ['YYYY-MM-DD HH:mm:ss', 'HH:mm:ss']).format('YYYY-MM-DD HH:mm:ss') : defaultValue;
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
