import React, {
  useState, useCallback, useRef, useEffect, ReactElement, useMemo,
} from 'react';
import { Button } from 'choerodon-ui/pro';
import { Dropdown } from 'choerodon-ui';
import {
  find, isEmpty, isEqual, isEqualWith, noop, omit,
} from 'lodash';
import { DropDownProps } from 'choerodon-ui/lib/dropdown';
import { ButtonProps } from 'choerodon-ui/pro/lib/button/Button';
import { observer, useComputed } from 'mobx-react-lite';
import { toJS, runInAction, observable } from 'mobx';
import { useCreation, usePersistFn, useWhyDidYouUpdate } from 'ahooks';
import { IFiledListItemProps, pageConfigApi } from '@/api';
import FieldList from './FieldList';
import ChoseFieldStore from './store';
import { IChosenFieldField, IChosenFieldFieldEvents } from './types';

interface Props {
  store: ChoseFieldStore,
  /** @default true */
  autoHiddenWhenEmpty?: boolean
  wrapClassName?: string,
  wrapStyle?: React.CSSProperties
  choseField?: (data: IChosenFieldField | IChosenFieldField[], status: 'add' | 'del') => void,
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
  /** 是否启用远程加载 @default true */
  server?: boolean
  defaultValue?: IChosenFieldField[],
  value?: Array<IChosenFieldField | string>, /** 可控value */
  events?: IChosenFieldFieldEvents,
  addFieldCallback?: (key: string) => void,
  dropDownProps?: Partial<DropDownProps>,
  dropDownBtnChildren?: ReactElement | ReactElement[] | string | null,
  dropDownBtnProps?: Partial<ButtonProps>,
}
interface IChoseFieldDataProps {
  store: ChoseFieldStore,
  fields: IChosenFieldField[],
}
export interface IChoseFieldComponentProps {
  store: ChoseFieldStore,
  choseField?: (data: IChosenFieldField | IChosenFieldField[], status: 'add' | 'del') => void,
  dropDownProps?: Partial<DropDownProps>,
  dropDownBtnChildren?: ReactElement | ReactElement[] | string | null,
  dropDownBtnProps?: Partial<ButtonProps>,
}
const defaultInitFieldAction = {
  initFieldStart: (data: any) => data,
  initField: (data: any) => data,
  initFieldFinish: (data: any) => data,
  initChosenField: (data: any) => data,

};
/**
 * 自定义比较两个值是否一致比较器
 * @param aValue
 * @param bValue
 * @returns
 */
function valueIsEqualCustomizer(aValue: any, bValue: any) {
  if (isEmpty(aValue) && isEmpty(bValue)) {
    return true;
  }
  return undefined;
}
export function useChoseField(config?: IChoseFieldConfig): [IChoseFieldDataProps, IChoseFieldComponentProps] {
  const [fields, setFields] = useState<IChosenFieldField[]>([]);
  const currentFields = useRef([] as any[]);
  const [value, setValue] = useState<string[] | undefined>(undefined);

  const loadData = async () => {
    const { content } = await pageConfigApi.load(); //
    setFields(content.map((item: IFiledListItemProps) => (item.system ? omit(item, 'id') : item)));
  };
  useEffect(() => {
    if (config?.server && typeof (config?.fields) === 'undefined') {
      loadData();
    } else if (config?.fields && !isEqual(config?.fields, currentFields.current)) {
      currentFields.current = config?.fields;
      setFields(currentFields.current);
    }
  }, [config?.fields, config?.server]);
  // 操作函数只初始化一次  防止方法多次创建 多次更改
  const events = useCreation(() => {
    let {
      initField, initChosenField, initFieldFinish, initFieldStart,
    }: IChosenFieldFieldEvents = defaultInitFieldAction;
    if (config?.events?.initChosenField) {
      initChosenField = config?.events?.initChosenField;
    }
    if (config?.events?.initField) {
      initField = config?.events?.initField;
    }
    if (config?.events?.initFieldStart) {
      initFieldStart = config?.events?.initFieldStart;
    }
    if (config?.events?.initFieldFinish) {
      initFieldFinish = config?.events?.initFieldFinish;
    }
    return {
      initField, initChosenField, initFieldFinish, initFieldStart, choseField: config?.events?.choseField || noop,
    };
  }, []);
  const addFieldCallback = usePersistFn(config?.addFieldCallback || noop);
  // 默认值只保存第一次传入
  const defaultValue = useMemo(() => config?.defaultValue, []);
  const store = useMemo(() => {
    const systemFields: Array<IChosenFieldField> = [];
    const customFields: Array<IChosenFieldField> = [];
    const currentChosenFields: Map<string, IChosenFieldField> = new Map();
    if (fields.length > 0) {
      events.initFieldStart(fields, currentChosenFields);
      fields.forEach((field) => {
        let newField = field;
        if (['time', 'datetime', 'date'].indexOf(field.fieldType ?? '') !== -1) {
          newField = { ...field, otherComponentProps: { range: true } };
        }
        const result = events.initField(newField, currentChosenFields);
        const { immutableCheck } = result || {};
        if (field.id && !field.system) {
          result && customFields.push(result);
          immutableCheck && currentChosenFields.set(field.code, result as IChosenFieldField);
        } else if (!field.noDisplay || field.system) {
          result && systemFields.push(result);
          immutableCheck && currentChosenFields.set(field.code, result as IChosenFieldField);
        }
      });
      defaultValue?.forEach((field) => {
        let newField = field;
        if (['time', 'datetime', 'date'].indexOf(field.fieldType ?? '') !== -1) {
          newField = { ...field, otherComponentProps: { range: true } };
        }
        const result = events.initChosenField(newField, currentChosenFields);
        result && currentChosenFields.set(field.code, result);
      });
      events.initFieldFinish(customFields, systemFields, currentChosenFields);
    }
    return new ChoseFieldStore({
      systemFields, customFields, chosenFields: [...currentChosenFields.values()], addFieldCallback,
    });
  }, [addFieldCallback, defaultValue, events, fields]);
  useEffect(() => {
    if (fields.length !== 0) {
      const nextValue = toJS(config?.value)?.map((item) => (typeof (item) === 'string' ? item : item.code)) || [];
      setValue((oldValue) => {
        if (!isEqualWith(nextValue, oldValue, valueIsEqualCustomizer)) {
          runInAction(() => {
            store.chosenFields = observable.map(nextValue.map((item) => {
              const temp = find(fields, { code: item })!;
              return [item, temp];
            }));
            store.selfUpdateCurrentOptionStatus();
          });
          return nextValue;
        }
        return oldValue;
      });
    }
  }, [config?.value, fields, fields.length, store]);
  const dataProps = {
    store,
    fields,
  };
  const componentProps: IChoseFieldComponentProps = {
    store,
    choseField: events.choseField,
    dropDownBtnChildren: config?.dropDownBtnChildren || '添加筛选',
    dropDownBtnProps: config?.dropDownBtnProps,
    dropDownProps: config?.dropDownProps,
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

const ChooseField: React.FC<Props> = (props) => {
  const [hidden, setHidden] = useState(true);
  const {
    dropDownBtnChildren = '添加筛选', wrapClassName, wrapStyle, store, choseField,
  } = props;
  const handleClickOut = useCallback(() => {
    setHidden(true);
  }, []);
  const ref = useClickOut(handleClickOut);
  const hiddenComponent = useComputed(() => store.getFields.filter(Boolean).every((item) => !item.length), [store]);
  return (
    <div className={wrapClassName} style={wrapStyle} hidden={hiddenComponent}>
      <Dropdown
        visible={!hidden}
        overlay={(
          <div
            role="none"
            ref={ref}
            onClick={(e) => {
              e.stopPropagation();
            }}
          >
            <FieldList store={store} closeMenu={() => setHidden(true)} onChose={choseField} />
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
          style={{
            marginLeft: 6,
          }}
          {...props.dropDownBtnProps}
        >
          {dropDownBtnChildren}
        </Button>

      </Dropdown>
    </div>
  );
};
ChooseField.defaultProps = {
  autoHiddenWhenEmpty: true,
};
export default observer(ChooseField);
