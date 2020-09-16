/* eslint-disable react/require-default-props */
/* eslint-disable jsx-a11y/click-events-have-key-events */
/* eslint-disable jsx-a11y/no-static-element-interactions */
import React, {
  useState, useCallback, useRef, useEffect, ReactElement,
} from 'react';
import {
  Button, Icon,
} from 'choerodon-ui/pro';
import { Dropdown } from 'choerodon-ui';
import { DropDownProps } from 'choerodon-ui/lib/dropdown';
import { observer } from 'mobx-react-lite';
import FieldList from './FieldList';
import ChoseFieldStore from './store';

interface Props {
  store: ChoseFieldStore,
  dropDownProps?: Partial<DropDownProps>,
  dropDownBtnChildren?: ReactElement | ReactElement[] | string | null,

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
        >
          {dropDownBtnChildren}
        </Button>

      </Dropdown>
    </div>
  );
}
export default observer(ChooseField);
