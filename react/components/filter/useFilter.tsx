/* eslint-disable no-case-declarations */
/* eslint-disable no-param-reassign */
import { useCallback } from 'react';
import { useImmerReducer } from 'use-immer';
import { pull, omit } from 'lodash';
import { IFieldType, ISystemFieldCode } from '@/common/types';
import { fieldApi } from '@/api';
import useDeepCompareEffect from '@/hooks/useDeepCompareEffect';
import IsInProgramStore from '@/stores/common/program/IsInProgramStore';

export interface ISystemField {
  code: ISystemFieldCode,
  title: string,
  fieldType: IFieldType,
  required?: boolean,
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

function getSystemFields(config?: FilterConfig): ISystemField[] {
  const systemFields: ISystemField[] = [
    // {
    //   code: 'issueIds',
    //   name: 'issueId',
    //
    //   noDisplay: true, // 不需要展示，仅作为一个筛选项
    // }, {
    //   code: 'quickFilterIds',
    //   name: '快速筛选',
    //
    //   noDisplay: true,
    // },
    // {
    //   code: 'contents',
    //   name: '概要',
    //
    //   noDisplay: true,
    // },
    {
      code: 'issueTypeId',
      title: '问题类型',
      fieldType: 'multiple',
      system: true,
    },
    {
      code: 'statusId',
      title: '状态',
      fieldType: 'multiple',
      system: true,
    },
    {
      code: 'assigneeId',
      title: '经办人',
      fieldType: 'member',
      system: true,
    },
    {
      code: 'reporterIds',
      title: '报告人',
      fieldType: 'member',
      system: true,
    },
    {
      code: 'sprint',
      title: '冲刺',
      fieldType: 'multiple',
      system: true,
    },
    {
      code: 'component',
      title: '模块',
      fieldType: 'multiple',
      system: true,
    },
    {
      code: 'label',
      title: '标签',
      fieldType: 'multiple',
      system: true,
    },
    {
      code: 'priorityId',
      title: '优先级',
      fieldType: 'multiple',
      system: true,
    },
    {
      code: 'version',
      title: '版本',
      fieldType: 'multiple',
      system: true,
    },
    {
      code: 'epic',
      title: '史诗',
      fieldType: 'multiple',
      system: true,
    },
    {
      code: 'feature',
      title: '特性',
      fieldType: 'multiple',
      system: true,
    },
    {
      code: 'createDate',
      title: '创建时间',
      fieldType: 'datetime',
      system: true,
    },
    {
      code: 'updateDate',
      title: '更新时间',
      fieldType: 'datetime',
      system: true,
    },
  ];
  const filtered = IsInProgramStore.isInProgram ? systemFields : systemFields.filter((f) => f.code !== 'feature');
  if (config?.systemFields) {
    if (Array.isArray(config.systemFields)) {
      return config.systemFields;
    } if (typeof config.systemFields === 'function') {
      return config.systemFields(filtered);
    }
    return filtered;
  }
  return filtered;
}
const initialState: FilterState = {
  selected: [],
  filter: {},
  systemFields: [],
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
export interface FilterConfig {
  selected?: string[]
  filter?: { [key: string]: any }
  systemFields?: ISystemField[] | ((systemFields: ISystemField[]) => ISystemField[])
  customFields?: ICustomField[]
}
function useFilter(config?: FilterConfig) {
  const [state, dispatch] = useImmerReducer<FilterState, FilterAction>(filterReducer, {
    ...initialState,
    ...omit(config, 'systemFields'),
    systemFields: getSystemFields(config),
  });
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
  }, [loadFields]);
  return {
    state,
    handleSelectChange,
    handleFilterChange,
  };
}

export default useFilter;
