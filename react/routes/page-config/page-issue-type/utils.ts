import { IFieldOptionProps } from '@/api';
import moment from 'moment';

function transformDefaultValue({
  fieldType, defaultValue, defaultValueObj, fieldOptions, optionKey = 'id',
}: { fieldType: string, defaultValue: any, defaultValueObj?: any, fieldOptions?: Array<IFieldOptionProps> | null, optionKey?: 'tempKey' | 'id' }) {
  if (!defaultValue && !defaultValueObj) {
    return defaultValue;
  }
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
      return fieldOptions?.filter((option) => valueArr.some((v) => v === option[optionKey])).map((item) => item.value).join(',') || defaultValue;
    }
    case 'member': {
      const { realName } = defaultValueObj || {};
      return realName || defaultValue;
    }
    default:
      return defaultValue;
  }
}

export { transformDefaultValue };
