import React, {
  useState, useCallback, useRef, useEffect, ReactElement, useMemo,
} from 'react';
import { Button } from 'choerodon-ui/pro';
import { Dropdown } from 'choerodon-ui';
import { omit } from 'lodash';
import { DropDownProps } from 'choerodon-ui/lib/dropdown';
import { ButtonProps } from 'choerodon-ui/pro/lib/button/Button';
import { observer } from 'mobx-react-lite';
import { IFiledListItemProps, pageConfigApi } from '@/api';
import FieldList from './FieldList';
import ChoseFieldStore from './store';
import { IChosenFieldField, IChosenFieldFieldEvents } from './types';

interface Props {
  store: ChoseFieldStore,
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
  defaultValue?: IChosenFieldField[],
  events?: IChosenFieldFieldEvents,
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
export function useChoseField(config?: IChoseFieldConfig): [IChoseFieldDataProps, IChoseFieldComponentProps] {
  const [fields, setFields] = useState<IChosenFieldField[]>([]);
  const loadData = async () => {
    const { content } = await pageConfigApi.load(); //
    setFields(content.map((item: IFiledListItemProps) => (item.system ? omit(item, 'id') : item)));
  };
  useEffect(() => {
    if (typeof (config?.fields) === 'undefined') {
      loadData();
    } else {
      setFields(config?.fields);
    }
  }, [config?.fields]);
  // 操作函数只初始化一次  防止方法多次创建 多次更改
  const events = useMemo(() => {
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
      initField, initChosenField, initFieldFinish, initFieldStart,
    };
  }, []);
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
        const result = events.initChosenField(newField, currentChosenFields);
        result && currentChosenFields.set(field.code, result);
      });
      events.initFieldFinish(customFields, systemFields, currentChosenFields);
    }

    return new ChoseFieldStore({ systemFields, customFields, chosenFields: [...currentChosenFields.values()] });
  }, [config?.defaultValue, events, fields]);
  const dataProps = {
    store,
    fields,
  };
  const componentProps: IChoseFieldComponentProps = {
    store,
    choseField: config?.events?.choseField,
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
            role="none"
            ref={ref}
            onClick={(e) => {
              e.stopPropagation();
            }}
          >
            <FieldList store={props.store} closeMenu={() => setHidden(true)} onChose={props.choseField} />
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
export default observer(ChooseField);
