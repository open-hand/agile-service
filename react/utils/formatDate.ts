import moment from 'moment';
import { MINUTE } from '@/constants/DATE_FORMAT';

export interface formatDateProps {
  value: string,
  text?: string,
  format?: string,
}

export const formatMinute = ({ value, text, format }: formatDateProps) => {
  if (value && moment(value).isValid()) {
    return moment(value).format(format ?? MINUTE);
  }
  return text ?? value;
};
