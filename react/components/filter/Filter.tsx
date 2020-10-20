import React, { useCallback } from 'react';
import { pull, uniq } from 'lodash';

interface IFilter {
  [key: string]: any
}
export interface FilterProps {
  value: string[]
  onChange: (value: string[]) => void
  filter: IFilter
  onFilterChange: (filter: IFilter) => void
}
const Filter: React.FC<FilterProps> = ({
  value, onChange, filter, onFilterChange,
}) => {
  const handleFilterChange = useCallback((code:string, v:any) => {
    const clonedFilter = { ...filter };
    clonedFilter[code] = v;
    onFilterChange(clonedFilter);
  }, []);
  const handleSelect = useCallback((select: string[]) => {
    const newValue = uniq([...value, ...select]);
    onChange(newValue);
  }, [onChange, value]);
  const handleUnSelect = useCallback((unselect: string[]) => {
    const newValue = [...value];
    const clonedFilter = { ...filter };
    let changedFilter = false;
    unselect.forEach((code) => {
      pull(newValue, code);
      if (Object.prototype.hasOwnProperty.call(filter, code)) {
        changedFilter = true;
        delete clonedFilter[code];
      }
    });
    onChange(newValue);
    if (changedFilter) {
      onFilterChange(clonedFilter);
    }
  }, [filter, onChange, onFilterChange, value]);

  return <div>filter</div>;
};

export default Filter;
