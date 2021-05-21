import { cloneDeep } from 'lodash';

export default function filterNullOption(search: any) {
  const filter = cloneDeep(search);
  filter.otherArgs.customField.option = filter.otherArgs.customField.option.filter((o: any) => o.value);
  return filter;
}
