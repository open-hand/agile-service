import { ListLayoutColumnVO } from '@/api';
import { IField } from '@/common/types';
import { find } from 'lodash';

export default function getListLayoutColumns(listLayoutColumns: ListLayoutColumnVO[] | null, fields: IField[]): ListLayoutColumnVO[] {
  let res:ListLayoutColumnVO[] = [];

  if (listLayoutColumns) {
    // TODO 过滤已被删除的字段
    res = [...listLayoutColumns];
  }
  fields.forEach(((field) => {
    if (!find(res, { columnCode: field.code })) {
      res.push({
        sort: 0,
        columnCode: field.code,
        display: false,
        fieldId: field.id,
      });
    }
  }));
  return res;
}