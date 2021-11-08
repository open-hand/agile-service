/* eslint-disable jsx-a11y/tabindex-no-positive */
/* eslint-disable jsx-a11y/no-noninteractive-tabindex */
import React, {
  useState, useRef, useCallback, useEffect, useImperativeHandle,
} from 'react';
import { observer } from 'mobx-react-lite';
import { Icon, Dropdown } from 'choerodon-ui/pro';

import { Action } from 'choerodon-ui/pro/lib/trigger/enum';
import classNames from 'classnames';
import { TemplateAction, templateApi } from '@/api';
import styles from './TemplateSelect.less';
import TemplateList from './components/list';
import { IFieldOption, ITemplate } from './components/edit/EditTemplate';
import { ITableColumnCheckBoxesDataProps } from '../table-column-check-boxes';

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
  templateSelectRef: React.MutableRefObject<{
    onOk: (template: ITemplate) => Promise<void>,
    templateList: ITemplate[],
    setTemplate: (template: ITemplate | undefined) => void
  } | undefined>
  selectTemplateOk: (codes: string[]) => void
  transformExportFieldCodes: (data: Array<string>, otherData: ITableColumnCheckBoxesDataProps) => Array<string>
  reverseTransformExportFieldCodes: (data: string[]) => string[]
  defaultInitCodes: string[]
  setTemplateFirstLoaded: (loaded: boolean) => void
}
const TemplateSelect: React.FC<Props> = (props) => {
  const {
    action, templateSelectRef, selectTemplateOk, transformExportFieldCodes, reverseTransformExportFieldCodes, defaultInitCodes, setTemplateFirstLoaded,
  } = props;
  const [templateList, setTemplateList] = useState<ITemplate[]>([]);
  const [selected, setSelected] = useState<ITemplate | undefined>();

  const [hidden, setHidden] = useState(true);

  const handleClickOut = useCallback(() => {
    setHidden(true);
  }, []);
  const ref = useClickOut(handleClickOut);

  const getTemplates = useCallback(async () => templateApi.getList(action).then((res: ITemplate[]) => {
    setTemplateList(res);
  }), [action]);

  useEffect(() => {
    const loadTemplates = async () => {
      await getTemplates();
      if (setTemplateFirstLoaded) {
        setTemplateFirstLoaded(true);
      }
    };
    loadTemplates();
  }, [getTemplates, setTemplateFirstLoaded]);

  const handleCreateOk = useCallback(async (template) => {
    await getTemplates();
    setSelected(template);
    selectTemplateOk(JSON.parse(template.templateJson));
  }, [getTemplates, selectTemplateOk]);

  useImperativeHandle(templateSelectRef, () => ({
    onOk: handleCreateOk,
    templateList,
    setTemplate: setSelected,
  }));

  const handleEditOk = useCallback(async (template) => {
    await getTemplates();
    if (selected?.id === template.id) {
      setSelected(template);
      selectTemplateOk(JSON.parse(template.templateJson));
    }
  }, [getTemplates, selectTemplateOk, selected?.id]);

  const handleDeleteOk = useCallback(async (id) => {
    await getTemplates();
    if (selected?.id === id) {
      setSelected(undefined);
      selectTemplateOk(JSON.parse(selected?.templateJson || JSON.stringify([]))); // 为了使重新判断模板是否存在
    }
  }, [getTemplates, selectTemplateOk, selected?.id, selected?.templateJson]);

  const handleClearTemplate = useCallback(() => {
    setSelected(undefined);
  }, [setSelected]);

  return (
    <div className={styles.template_select}>
      <Dropdown
        visible={!hidden}
        // @ts-ignore
        // getPopupContainer={(triggerNode) => document.querySelector('#template_select')}
        overlay={(
          <div
            // @ts-ignore
            ref={ref}
            role="none"
            onClick={(e) => {
              e.stopPropagation();
            }}
          >
            <TemplateList
              {...props}
              templateList={templateList}
              selected={selected}
              setSelected={setSelected}
              templateItemNameCls={templateItemNameCls}
              onEdit={handleEditOk}
              onDelete={handleDeleteOk}
              selectTemplateOk={selectTemplateOk}
              transformExportFieldCodes={transformExportFieldCodes}
              reverseTransformExportFieldCodes={reverseTransformExportFieldCodes}
              defaultInitCodes={defaultInitCodes}
              setHidden={setHidden}
            />
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
              top: selected ? '3px' : '14px',
              left: '6px',
              fontSize: selected ? '12px' : '13px',
            }}
          >
            选择模板
          </span>
          <span className={styles.selected}>
            {selected?.name}
          </span>
          {
            selected && (
              <Icon
                type="cancel"
                className={classNames(styles.clear_icon, {
                  [styles.hasSelected_clearIcon]: selected,
                })}
                onClick={handleClearTemplate}
              />
            )
          }
          <Icon type="expand_more" className={classNames(styles.iconPicker, { [styles.hasSelected_iconPicker]: selected })} />
        </div>
      </Dropdown>
    </div>
  );
};

export default observer(TemplateSelect);
