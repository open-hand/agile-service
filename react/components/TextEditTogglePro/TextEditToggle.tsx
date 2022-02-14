import React, {
  useState, useRef, cloneElement, useEffect, Fragment,
} from 'react';
import FormField from 'choerodon-ui/pro/lib/field';
import TriggerField from 'choerodon-ui/pro/lib/trigger-field';
import { toJS } from 'mobx';
import {
  isEqual, isEqualWith, isNull, isUndefined,
} from 'lodash';
import classNames from 'classnames';
import { useCreation } from 'ahooks';
import useClickOut from '@/hooks/useClickOut';
import styles from './TextEditToggle.less';

interface RenderProps {
  value: any
  editing: boolean
}
interface EditorRender {
  submit: () => void
  hideEditor: () => void
}
export type Action = 'click' | 'blur' | 'change'
interface Props {
  disabled?: boolean
  submitTrigger?: Action[] // 触发提交的动作
  alwaysRender?: boolean // 查看模式也挂载编辑器
  mountRenderEditor?: boolean // 第一次mount就挂载编辑器，设为false就会在第一次编辑时再挂载，相当于懒加载了
  editor: (editorRender: EditorRender) => JSX.Element
  editorExtraContent?: () => JSX.Element
  children: (({ value, editing }: RenderProps) => React.ReactNode) | React.ReactNode
  className?: string
  onSubmit: (data: any) => void
  initValue: any
}
/**
 * 统一化空值
 * @param val
 * @returns 空值/未定义值/空字符串 返回 null
 */
function uniformEmptyValue(val: any): any {
  if (isNull(val) || isUndefined(val) || val === '') {
    return null;
  }
  return val;
}
function customizer(val1: any, val2: any) {
  // 空值判断
  return uniformEmptyValue(val1) === uniformEmptyValue(val2) || undefined;
}
const TextEditToggle: React.FC<Props> = ({
  disabled, submitTrigger = ['blur'], editor, editorExtraContent, children: text, className, onSubmit, initValue, alwaysRender = true, mountRenderEditor = true,
} = {} as Props) => {
  const [editing, setEditing] = useState(false);
  const [value, setValue] = useState(initValue);
  const editingRef = useRef(editing);
  const dataRef = useRef(initValue);
  const editorRef = useRef<JSX.Element | FormField<any>>(null);
  const firstRenderEditorRef = useRef(true);

  const handleClickOut = () => {
    if (submitTrigger.includes('click')) {
      submit();
    }
  };
  const containerRef = useClickOut<HTMLDivElement>(handleClickOut);
  const containerSize = useCreation(() => ({
    width: containerRef.current?.getBoundingClientRect().width,
    height: containerRef.current?.getBoundingClientRect().height,
  }), [editing]);
  editingRef.current = editing;
  useEffect(() => {
    dataRef.current = initValue;
    setValue(initValue);
  }, [initValue]);
  useEffect(() => {
    // 自动聚焦
    if (editing && editorRef.current) {
      // @ts-ignore
      editorRef.current.focus();
    }
  });
  const hideEditor = () => {
    if (editing) {
      if (containerRef.current) {
        containerRef.current.blur();
      }
      const { trigger } = editorRef.current as unknown as TriggerField<any> || {};
      // 如果属于下拉框类型组件 则立刻关闭下拉框 避免飘逸现象
      if (trigger) {
        // trigger.cancelPopupTask();
        trigger.setPopupHidden(true);
      }
      // 延迟一会隐藏
      setEditing(false);
    }
  };
  const showEditor = () => {
    firstRenderEditorRef.current = false;
    if (!editing) {
      setEditing(() => true);
    }
  };
  const handleChange = (originOnChange: Function | undefined) => (newValue: any) => {
    dataRef.current = newValue;
    setValue(newValue);
    if (originOnChange) {
      originOnChange(newValue);
    }
    if (submitTrigger.includes('change')) {
      submit(newValue);
    }
  };
  const handleEditorBlur = (originOnBlur: Function) => () => {
    if (originOnBlur) {
      originOnBlur();
    }
    if (submitTrigger.includes('blur')) {
      submit();
    }
  };
  const submit = (newValue?: any) => {
    let waitSubmitValue = (editorRef.current as any)?.getValue
      ? toJS((editorRef.current as any)?.getValue()) : dataRef.current;
    if (typeof (newValue) !== 'undefined') {
      waitSubmitValue = newValue;
    }
    // 延缓submit，因为有时候blur之后才会onchange，保证拿到的值是最新的
    setTimeout(async () => {
      // @ts-ignore
      if (editingRef.current && editorRef.current && await editorRef.current?.validate(waitSubmitValue)) {
        if (containerRef.current) {
          containerRef.current.blur();
        }
        hideEditor();
        if (!isEqualWith(waitSubmitValue, initValue, customizer)) {
          onSubmit(waitSubmitValue);
        }
      }
    });
  };
  const renderEditor = () => {
    const editorElement = typeof editor === 'function' ? editor({ submit, hideEditor }) : editor;
    if (!editing && firstRenderEditorRef.current && !mountRenderEditor) {
      return null;
    }
    if (!editing && !alwaysRender) {
      return null;
    }
    const extraContent = typeof editorExtraContent === 'function' ? editorExtraContent() : editorExtraContent;
    const originProps = editorElement.props;
    const editorProps: any = {
      value,
      onChange: handleChange(originProps.onChange),
      onBlur: handleEditorBlur(originProps.onBlur),
      ref: editorRef,
    };
    if (containerRef.current) {
      editorProps.style = {
        ...containerSize,
        ...originProps.style,
      };
    }
    return (
      <>
        {cloneElement(editorElement, editorProps)}
        {extraContent}
      </>
    );
  };
  const renderText = () => {
    // @ts-ignore
    const textElement = typeof text === 'function' ? text({ value: dataRef.current, editing }) : text;
    return textElement;
  };
  const getCellRenderer = () => (
    <>
      {/* 在没编辑的时候也会渲染，目的是提前加载数据 */}
      {!disabled && (
        <div
          className={classNames(styles.editor, {
            [styles.hidden]: !editing,
          })}
        >
          {renderEditor()}
        </div>
      )}
      <div className={classNames(styles.text, {
        [styles.hidden]: editing,
      })}
      >
        {renderText()}
      </div>
    </>
  );
  const handleFocus = () => {
    if (!disabled) {
      showEditor();
    }
  };
  return (
    <div
      ref={containerRef}
      className={classNames(
        styles.container,
        { [styles.disabled]: disabled },
        className,
      )}
      // eslint-disable-next-line jsx-a11y/no-noninteractive-tabindex
      tabIndex={0}
      onFocus={handleFocus}
    >
      {getCellRenderer()}
    </div>
  );
};

export default TextEditToggle;
