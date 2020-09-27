/* eslint-disable react/require-default-props */
/* eslint-disable jsx-a11y/click-events-have-key-events */
/* eslint-disable jsx-a11y/no-static-element-interactions */
import React, {
  useState, useCallback, useRef, useEffect, ReactElement, useMemo,
} from 'react';
import {
  Button, Icon,
} from 'choerodon-ui/pro';
import { Dropdown } from 'choerodon-ui';
import { DropDownProps } from 'choerodon-ui/lib/dropdown';
import { ButtonProps } from 'choerodon-ui/pro/lib/button/Button';
import { observer } from 'mobx-react-lite';
import FieldList, { useChoseFieldStore } from './FieldList';
import ChoseFieldStore from './store';
import { IChosenFieldField, IChosenFieldFieldActions } from './types';

interface Props {
  store: ChoseFieldStore,
  dropDownProps?: Partial<DropDownProps>,
  dropDownBtnChildren?: ReactElement | ReactElement[] | string | null,
  dropDownBtnProps?: Partial<ButtonProps>,
}
/**
 *@param fields 加载的字段列表
 *@param defaultValue 默认选中
 *@param actions 数据初始化过程中额外操作
 */
interface IChoseFieldConfig {
  fields?: IChosenFieldField[],
  defaultValue?: IChosenFieldField[],
  actions?: IChosenFieldFieldActions,
}
interface IChoseFieldDataProps {
  store: ChoseFieldStore,
  fields: IChosenFieldField[],
}
interface IChoseFieldComponentProps {
  store: ChoseFieldStore,
}
const defaultInitFieldAction = {
  initField: (data: any) => data,
  initChosenField: (data: any) => data,
};
export function useChoseField(config?: IChoseFieldConfig): [IChoseFieldDataProps, IChoseFieldComponentProps] {
  const [fields, setFields] = useState<IChosenFieldField[]>([]);
  const loadData = async () => {
    console.log('loadData...');
    return [];
  };
  useEffect(() => {
    if (typeof (config?.fields) === 'undefined') {
      loadData();
    } else {
      setFields(config?.fields);
    }
  }, [config?.fields]);
  const actions = useMemo(() => {
    let { initField, initChosenField }: IChosenFieldFieldActions = defaultInitFieldAction;
    if (config?.actions?.initChosenField) {
      initChosenField = config?.actions?.initChosenField;
    }
    if (config?.actions?.initField) {
      initField = config?.actions?.initField;
    }
    return { initField, initChosenField };
  }, []);
  const store = useMemo(() => {
    const systemFields: Array<IChosenFieldField> = [];
    const customFields: Array<IChosenFieldField> = [];
    const currentChosenFields: Map<string, IChosenFieldField> = new Map();
    fields.forEach((field) => {
      let newField = field;
      if (['time', 'datetime', 'date'].indexOf(field.fieldType ?? '') !== -1) {
        newField = { ...field, otherComponentProps: { range: true } };
      }
      const result = actions.initField(newField, currentChosenFields);
      const { immutableCheck } = result || {};
      if (field.id) {
        result && customFields.push(result);
        immutableCheck && currentChosenFields.set(field.code, result as IChosenFieldField);
      } else if (!field.noDisplay) {
        result && systemFields.push(result);
        immutableCheck && currentChosenFields.set(field.code, result as IChosenFieldField);
      }
    });
    config?.defaultValue?.forEach((field) => {
      let newField = field;
      if (['time', 'datetime', 'date'].indexOf(field.fieldType ?? '') !== -1) {
        newField = { ...field, otherComponentProps: { range: true } };
      }
      const result = actions.initChosenField(newField, currentChosenFields);
      result && currentChosenFields.set(field.code, newField);
    });
    return new ChoseFieldStore({ systemFields, customFields, chosenFields: [...currentChosenFields.values()] });
  }, [actions, fields]);
  const dataProps = {
    store,
    fields,
  };
  const componentProps = {
    store,
  };
  return [dataProps, componentProps];
}
function useClickOut(onClickOut: (e?: any) => void) {
  const ref = useRef<HTMLDivElement | null>(null);
  const handleClick = useCallback((e) => {
    if (ref.current && !ref.current.contains(e.target)) {
      onClickOut(e);
    }
  }, [onClickOut]);
  useEffect(() => {
    document.addEventListener('click', handleClick);
    return () => {
      document.removeEventListener('click', handleClick);
    };
  }, [handleClick]);
  return ref;
}

function ChooseField(props: Props) {
  const [hidden, setHidden] = useState(true);
  const { dropDownBtnChildren = '添加筛选' } = props;
  const handleClickOut = useCallback(() => {
    setHidden(true);
  }, []);
  const ref = useClickOut(handleClickOut);

  return (
    <div>
      <Dropdown
        getPopupContainer={(trigger) => trigger.parentNode as HTMLElement}
        visible={!hidden}
        overlay={(
          <div
            ref={ref}
            onClick={(e) => {
              e.stopPropagation();
            }}
          >
            <FieldList store={props.store} />
          </div>
        )}
        trigger={['click']}
        {...props.dropDownProps}
      >
        <Button
          onClick={(e) => {
            e.nativeEvent.stopImmediatePropagation();
            setHidden(false);
          }}
          {...props.dropDownBtnProps}
        >
          {dropDownBtnChildren}
        </Button>

      </Dropdown>
    </div>
  );
}
export default observer(ChooseField);
