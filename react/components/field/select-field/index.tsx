import React, {
  useState, useCallback, useRef, useEffect,
} from 'react';
import { Button, Dropdown } from 'choerodon-ui/pro';

import { pull, uniq } from 'lodash';
import FieldList from './FieldList';
import useFormatMessage from '@/hooks/useFormatMessage';

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
export interface ISelectableField<T> {
  code: T
  title: string
  disabled?: boolean
}
export interface Group {
  title: string
  options: {
    title: string
    code: string,
    disabled?: boolean
  }[]
}
export interface SelectFieldProps {
  value?: string[]
  onChange?: (codes: string[], select: boolean, value: string[]) => void
  groups: Group[]
  triggerElement?: React.ReactElement
}
const SelectField: React.FC<SelectFieldProps> = ({
  groups,
  onChange,
  triggerElement,
  ...otherProps
}) => {
  const formatMessage = useFormatMessage('agile.common');

  const [hidden, setHidden] = useState(true);
  const [value, setValue] = useState<string[]>([]);
  const controlled = 'value' in otherProps;
  const realValue = controlled ? otherProps.value as string[] : value;
  const handleClickOut = useCallback(() => {
    setHidden(true);
  }, []);
  const ref = useClickOut(handleClickOut);
  const handleSelect = useCallback((code: string | string[]) => {
    const codes = Array.isArray(code) ? code : [code];
    setValue((v) => {
      const result = uniq([...v, ...codes]);
      onChange && onChange(codes, true, result);
      if (controlled) {
        return v;
      }
      return result;
    });
  }, [controlled, onChange]);
  const handleUnSelect = useCallback((code: string | string[]) => {
    const codes = Array.isArray(code) ? code : [code];
    setValue((v) => {
      const newValues = [...v];
      pull(newValues, ...codes);
      onChange && onChange(codes, false, newValues);
      if (controlled) {
        return v;
      }
      return newValues;
    });
  }, [controlled, onChange]);

  return (
    <div>
      <Dropdown
        visible={!hidden}
        overlay={(
          <div
            role="none"
            ref={ref}
            onMouseDown={(e) => {
              e.stopPropagation();
            }}
          >
            <FieldList
              groups={groups}
              closeMenu={() => setHidden(true)}
              value={realValue}
              onSelect={handleSelect}
              onUnSelect={handleUnSelect}
            />
          </div>
        )}
        trigger={['click'] as any}
      >
        {triggerElement ? React.cloneElement(triggerElement, {
          onClick: (e: any) => {
            e.nativeEvent.stopImmediatePropagation();
            setHidden(false);
          },
        }) : (
          <Button
            icon="add"
            onClick={(e) => {
              e.nativeEvent.stopImmediatePropagation();
              setHidden(false);
            }}
          >
            {formatMessage({ id: 'add.filter' })}
          </Button>
        )}
      </Dropdown>
    </div>
  );
};
export default SelectField;
