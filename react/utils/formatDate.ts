import moment from 'moment';
import { FORMAT_FIELDS, MINUTE } from '@/constants/DATE_FORMAT';

export interface FormatDateProps {
  value: string,
  text?: string,
  format?: string,
}

export interface FormatFieldDateValueProps extends FormatDateProps {
  fieldCode: string,
}

export const formatMinute = ({ value, text, format }: FormatDateProps) => {
  if (value && moment(value).isValid()) {
    return moment(value).format(format ?? MINUTE);
  }
  return text ?? value;
};

export const formatFieldDateValue = ({ fieldCode, value, ...other }: FormatFieldDateValueProps) => {
  if (fieldCode && FORMAT_FIELDS.includes(fieldCode)) {
    return formatMinute({ value, ...other });
  }
  return value;
};
