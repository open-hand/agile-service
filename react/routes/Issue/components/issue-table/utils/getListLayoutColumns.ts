import { find, includes } from 'lodash';
import type { ListLayoutColumnVO } from '../../../../../api';
import type{ IFoundationHeader } from '../../../../../common/types';

export interface ListLayoutColumnItem extends ListLayoutColumnVO {
  /** 等同于 label */
  title: string
  /**  字段的label */
  label: string
  /** 是否为删除字段 默认返回非删除的字段 */
  isDel: boolean
  /** 字段的额外配置 */
  extraConfig?: any
}
/**
* 获取列表展示字段
* @param listLayoutColumns 当前列配置字段列表
* @param fields 所有子弹
* @param alwaysShowCodes 总是渲染的字段 默认会过滤
* @returns
*/
export default function getListLayoutColumns(listLayoutColumns: ListLayoutColumnVO[] | null, fields: IFoundationHeader[], alwaysShowCodes = []): Array<ListLayoutColumnItem> {
  const hasFieldInList = new Set<string>();
  const res: Array<ListLayoutColumnItem> = (listLayoutColumns || []).map((item) => {
    const tableField = find(fields, { code: item.columnCode });
    const isDel = !tableField;
    if (!isDel) {
      hasFieldInList.add(item.columnCode);
    }
    return {
      ...item, title: tableField?.title!, label: tableField?.title!, isDel,
    };
  });
  for (const field of fields) {
    if (!hasFieldInList.has(field.code) && !includes(alwaysShowCodes, field.code)) {
      res.push({
        width: 0,
        sort: 0,
        columnCode: field.code,
        display: false,
        label: field.title,
        title: field.title,
        isDel: false,
      });
    }
  }
  return res.filter((item) => !item.isDel);
}
