import { find } from 'lodash';
import { ListLayoutColumnVO } from '@/api';
import { IFoundationHeader } from '@/common/types';

export default function getColumnManageOptions(listLayoutColumns: ListLayoutColumnVO[] | null, fields: IFoundationHeader[]): IFoundationHeader[] {
  const res: IFoundationHeader[] = [];

  if (!listLayoutColumns) {
    return fields;
  }
  listLayoutColumns.forEach((layoutColumn) => {
    const { columnCode: code, width } = layoutColumn;
    const option = find(fields, { code });
    if (option) {
      res.push(option);
    }
  });
  fields.forEach(((field) => {
    if (!find(res, { code: field.code })) {
      res.push(field);
    }
  }));
  return res;
}
