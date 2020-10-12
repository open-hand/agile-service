/* eslint-disable no-case-declarations */
/* eslint-disable no-param-reassign */
import { useCallback } from 'react';
import { useImmerReducer } from 'use-immer';
import { pull } from 'lodash';
import { IFieldType, ISystemFieldCode } from '@/common/types';
import { fieldApi } from '@/api';
import useDeepCompareEffect from '@/hooks/useDeepCompareEffect';

export interface ISystemField {
  code: ISystemFieldCode,
  title: string,
  fieldType: IFieldType,
  required: boolean,
  system: true
}
interface IFieldOption {
  id: string
  value: string
  enabled: boolean
}
export interface ICustomField {
  id: string
  code: string
  title: string
  fieldType: IFieldType
  required: boolean
  fieldOptions?: IFieldOption[]
  value?: string
  valueStr?: string
  system: boolean
}
export type IFilterField = ISystemField | ICustomField

const defaultSystemFields: ISystemField[] = [{
  code: 'sprint',
  title: '冲刺',
  fieldType: 'single',
  required: true,
  system: true,
}, {
  code: 'issueTypeId',
  title: '问题类型',
  fieldType: 'single',
  required: false,
  system: true,
}];
const initialState: FilterState = {
  selected: [],
  filter: {},
  systemFields: defaultSystemFields,
  customFields: [],
};
interface FilterState {
  selected: string[]
  filter: { [key: string]: any }
  systemFields: ISystemField[]
  customFields: ICustomField[]
}
type FilterAction =
  | {
    type: 'SET_CUSTOM_FIELDS', payload: ICustomField[]
  }
  | {
    type: 'CHANGE', payload: {
      codes: string[]
      select: boolean
    }
  }
  | {
    type: 'FILTER_CHANGE', payload: {
      code: string
      value: any
    }
  };

function filterReducer(draft: FilterState, action: FilterAction): FilterState {
  switch (action.type) {
    case 'SET_CUSTOM_FIELDS': {
      const customFields = action.payload;
      draft.customFields = customFields;
      return draft;
    }
    case 'CHANGE': {
      const { select, codes } = action.payload;
      if (select) {
        codes.forEach((code) => {
          draft.selected.push(code);
        });
      } else {
        codes.forEach((code) => {
          pull(draft.selected, code);
          if (Object.prototype.hasOwnProperty.call(draft.filter, code)) {
            delete draft.filter[code];
          }
        });
      }
      return draft;
    }

    case 'FILTER_CHANGE': {
      const { code, value } = action.payload;
      draft.filter[code] = value;
      return draft;
    }

    default: return draft;
  }
}
interface FilterConfig extends Partial<FilterState> {

}
function useFilter(config?: FilterConfig) {
  const [state, dispatch] = useImmerReducer<FilterState, FilterAction>(filterReducer, initialState, () => ({
    ...initialState,
    ...config,
  }));
  const handleSelectChange = useCallback((codes: string[], select: boolean) => {
    dispatch({
      type: 'CHANGE',
      payload: {
        codes,
        select,
      },
    });
  }, [dispatch]);
  const handleFilterChange = useCallback((code: string, value: any) => {
    dispatch({
      type: 'FILTER_CHANGE',
      payload: {
        code,
        value,
      },
    });
  }, [dispatch]);
  const loadFields = useCallback(async () => {
    const Fields = (await fieldApi.getCustomFields()).map((f: any) => ({
      ...f,
      code: f.id,
      title: f.fieldTypeName,
    }));
    dispatch({
      type: 'SET_CUSTOM_FIELDS',
      payload: Fields,
    });
  }, [dispatch]);
  useDeepCompareEffect(() => {
    if (!config || (config && !('customFields' in config))) {
      loadFields();
    }
  }, [config, loadFields]);
  return {
    state,
    handleSelectChange,
    handleFilterChange,
  };
}

export default useFilter;
