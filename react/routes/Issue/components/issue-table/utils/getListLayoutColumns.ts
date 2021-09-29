import { find } from 'lodash';
import { ListLayoutColumnVO } from '@/api';
import { IFoundationHeader } from '@/common/types';

export default function getListLayoutColumns(listLayoutColumns: ListLayoutColumnVO[] | null, fields: IFoundationHeader[]): ListLayoutColumnVO[] {
  let res:ListLayoutColumnVO[] = [];

  if (listLayoutColumns) {
    // TODO 过滤已被删除的字段
    res = [...listLayoutColumns];
  }
  fields.forEach(((field) => {
    if (!find(res, { columnCode: field.code })) {
      res.push({
        width: 0,
        sort: 0,
        columnCode: field.code,
        display: false,
        // fieldId: field.id,
      });
    }
  }));
  return res;
}
