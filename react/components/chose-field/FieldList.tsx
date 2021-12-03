import React, { useMemo } from 'react';
import { observer } from 'mobx-react-lite';
import {
  CheckBox, Button, TextField, Icon, Tooltip,
} from 'choerodon-ui/pro';
import './FieldList.less';
import { useDebounceFn, usePersistFn } from 'ahooks';
import { IChosenFieldField, IUseChoseFieldProps } from './types';
import ChoseFieldStore from './store';
import useFormatMessage from '@/hooks/useFormatMessage';

const prefix = 'c7n-agile-choose-field-list';

export function useChoseFieldStore(props: IUseChoseFieldProps) {
  return useMemo(() => new ChoseFieldStore(props), [props]);
}
interface Props {
  store: ChoseFieldStore,
  closeMenu: () => void,
  onChose: ((v: IChosenFieldField | IChosenFieldField[], status: 'add' | 'del') => void) | undefined
}
function FieldList({ store, closeMenu, onChose }: Props) {
  const currentOptionStatus = store.getCurrentOptionStatus;
  const [systemFields, customFields] = store.getFields;
  const formatMessage = useFormatMessage();

  function handleChange(value: string | undefined, field: IChosenFieldField) {
    if (value) {
      store.addChosenFields(value, field);
      // 延迟处理
      setTimeout(() => onChose && onChose(field, 'add'), 50);
    } else {
      store.delChosenFields(field.code);
      // 延迟处理 避免选择卡顿
      setTimeout(() => onChose && onChose(field, 'del'), 60);
    }
  }
  const { run: handleInput } = useDebounceFn((value: string) => {
    store.setSearchVal(value);
  }, { wait: 540 });
  const handleTooltipMouseEnter = usePersistFn(
    (e, title: string) => {
      console.log('handleTooltipMouseEnter', title);
      return Tooltip.show(e.target, {
        title,
        placement: 'topLeft',
      });
    },
  );
  const handleTooltipMouseLeave = usePersistFn(() => Tooltip.hide());
  return (
    <div
      className={prefix}
    >
      <div className={`${prefix}-search`}>
        <TextField
          onInput={(e: any) => handleInput(e.target.value)}
          style={{ flex: 1 }}
          value={store.getSearchVal}
          onChange={(v) => {
            store.setSearchVal(v);
          }}
          prefix={<Icon type="search" />}
          placeholder={formatMessage({ id: 'agile.common.search.placeholder' }) as string}
          clearButton
        />
      </div>
      <div className={`${prefix}-header`}>
        <CheckBox
          indeterminate={currentOptionStatus === 'PART'}
          checked={currentOptionStatus === 'ALL'}
          onChange={(checkAll) => {
            if (checkAll) {
              closeMenu(); // 避免焦点丢失时无法再次点击添加筛选
              const data = store.addAllChosenFields();
              onChose && onChose(data, 'add');
            } else {
              const data = store.cancelAllChosenFields();
              onChose && onChose(data, 'del');
            }
          }}
        >
          {formatMessage({ id: 'agile.common.all' })}
        </CheckBox>
        <Button
          style={{ display: currentOptionStatus !== 'NONE' ? 'inline-block' : 'none' }}
          onClick={() => {
            const data = store.cancelAllChosenFields();
            onChose && onChose(data, 'del');
          }}
        >
          {formatMessage({ id: 'agile.search.clear.filter' })}
        </Button>
      </div>
      <div className={`${prefix}-content`}>
        {systemFields.length > 0 && (
          <div className={`${prefix}-section`}>
            <div className={`${prefix}-title`}>预定义字段</div>
            <div className={`${prefix}-list`}>
              {systemFields.map((field) => {
                const { code, nameKey } = field;
                const name = nameKey ? formatMessage({ id: nameKey as any, defaultMessage: field.name }) as string : field.name;
                const disabled = typeof (field.immutableCheck) !== 'undefined';
                return (
                  <div className={`${prefix}-item`} key={code} onMouseEnter={(e) => handleTooltipMouseEnter(e, name)} onMouseLeave={handleTooltipMouseLeave}>
                    <CheckBox
                      value={code}
                      disabled={disabled}
                      checked={disabled ? field.immutableCheck : !!store.getChosenByCode(code)}
                      onChange={(value) => handleChange(value, field)}
                    >
                      {name}
                    </CheckBox>
                  </div>

                );
              })}

            </div>
          </div>
        )}
        {customFields.length > 0 && (
          <div className={`${prefix}-section`}>
            <div className={`${prefix}-title`}>自定义字段</div>
            <div className={`${prefix}-list`}>
              {customFields.map((field) => {
                const { name, code } = field;
                const disabled = typeof (field.immutableCheck) !== 'undefined';
                return (
                  <Tooltip title={name}>
                    <div className={`${prefix}-item`} key={code}>
                      <CheckBox
                        value={code}
                        disabled={disabled}
                        checked={disabled ? field.immutableCheck : !!store.getChosenByCode(code)}
                        onChange={(value) => handleChange(value, field)}
                      >
                        {name}
                      </CheckBox>
                    </div>
                  </Tooltip>
                );
              })}
            </div>
          </div>
        )}
      </div>
    </div>
  );
}
export default observer(FieldList);
