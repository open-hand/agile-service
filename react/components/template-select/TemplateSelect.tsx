/* eslint-disable jsx-a11y/tabindex-no-positive */
/* eslint-disable jsx-a11y/no-noninteractive-tabindex */
import React, {
  useState, useRef, useCallback, useEffect,
} from 'react';
import { observer } from 'mobx-react-lite';
import { Icon } from 'choerodon-ui';
import { Dropdown } from 'choerodon-ui/pro';

import { Action } from 'choerodon-ui/pro/lib/trigger/enum';
import useIsProgram from '@/hooks/useIsProgram';
import { TemplateAction, templateApi } from '@/api';
import styles from './TemplateSelect.less';
import TemplateList from './components/list';
import { IFieldOption, ITemplate } from './components/edit/EditTemplate';

const templateItemNameCls = 'c7n-templateItem-name';
// @ts-ignore
function useClickOut(onClickOut) {
  const ref = useRef();
  const handleClick = useCallback((e) => {
    const popupContainerEles = document.getElementsByClassName('c7n-pro-popup-container');
    const triggerBtn = document.getElementsByClassName(styles.dropDown_trigger)[0];
    let allIsNotContain = true;
    for (let i = 0; i < popupContainerEles.length; i += 1) {
      if (popupContainerEles[i].contains(e.target)) {
        allIsNotContain = false;
        break;
      }
    }
    // @ts-ignore
    if (e.target.className?.indexOf(templateItemNameCls) > -1 || (ref.current && (!ref.current.contains(e.target) && allIsNotContain && e.target.tagName !== 'BODY' && !triggerBtn.contains(e.target)))) {
      onClickOut(e);
    }
  }, [onClickOut]);
  useEffect(() => {
    document.addEventListener('click', handleClick, true);
    return () => {
      document.removeEventListener('click', handleClick, true);
    };
  }, [handleClick]);
  return ref;
}

interface Props {
  action: TemplateAction
  checkOptions: IFieldOption[]
}
const TemplateSelect: React.FC<Props> = (props) => {
  const { action } = props;
  const { isProgram } = useIsProgram();
  console.log('isProgram：');
  console.log(isProgram);

  const [templateList, setTemplateList] = useState<ITemplate[]>([{
    id: '1',
    name: '模板1模板1模板1模板1',
  }, {
    id: '2',
    name: '模板2',
  }, {
    id: '3',
    name: '模板3',
  }]);
  const [selected, setSelected] = useState<ITemplate | undefined>();

  const [hidden, setHidden] = useState(true);

  const handleClickOut = useCallback(() => {
    setHidden(true);
  }, []);
  const ref = useClickOut(handleClickOut);

  useEffect(() => {
    templateApi.getList(action).then((res: ITemplate[]) => {
      setTemplateList(res);
    });
  }, [action]);

  return (
    <div className={styles.template_select}>
      <Dropdown
          // @ts-ignore
        getPopupContainer={(trigger) => trigger.parentNode}
        visible={!hidden}
        overlay={(
          <div
            // @ts-ignore
            ref={ref}
            role="none"
            onClick={(e) => {
              e.stopPropagation();
            }}
          >
            <TemplateList {...props} templateList={templateList} setSelected={setSelected} templateItemNameCls={templateItemNameCls} />
          </div>
          )}
        trigger={['click'] as Action[]}
      >
        <div
          // eslint-disable-next-line jsx-a11y/tabindex-no-positive
          // @ts-ignore
          tabIndex={1}
          className={`${styles.dropDown_trigger} ${selected ? styles.dropDown_trigger_hasSelected : styles.dropDown_trigger_hasNoSelected}`}
          role="none"
          onClick={(e) => {
            e.nativeEvent.stopImmediatePropagation();
            setHidden(!hidden);
          }}
        >
          <span
            className={styles.trigger_label}
            style={{
              top: selected ? '-12px' : '7px',
              left: '6px',
              fontSize: selected ? '12px' : '13px',
            }}
          >
            选择模板
          </span>
          <span className={styles.selected}>
            {selected?.name}
          </span>
          <Icon type="arrow_drop_down" className={styles.iconPicker} />
        </div>
      </Dropdown>
    </div>
  );
};

export default observer(TemplateSelect);
