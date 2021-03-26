import React, {
  useState, useRef, cloneElement, useEffect, Fragment,
} from 'react';
import FormField from 'choerodon-ui/pro/lib/field';
import { toJS } from 'mobx';
import classNames from 'classnames';
import useClickOut from '@/hooks/useClickOut';
import styles from './TextEditToggle.less';

interface RenderProps {
  value: any
  editing: boolean
}
interface EditorRender {
  submit: () => void
}
export type Action = 'click' | 'blur' | 'change'
interface Props {
  disabled?: boolean
  submitTrigger?: Action[] // 触发提交的动作
  alwaysRender?: boolean // 查看模式也挂载编辑器
  editor: (editorRender: EditorRender) => JSX.Element
  editorExtraContent?: () => JSX.Element
  children: (({ value, editing }: RenderProps) => React.ReactNode) | React.ReactNode
  className?: string
  onSubmit: (data: any) => void
  initValue: any
}

const TextEditToggle: React.FC<Props> = ({
  disabled, submitTrigger = ['blur'], editor, editorExtraContent, children: text, className, onSubmit, initValue, alwaysRender = true,
} = {} as Props) => {
  const [editing, setEditing] = useState(false);
  const [value, setValue] = useState(initValue);
  const editingRef = useRef(editing);
  const dataRef = useRef(initValue);
  const editorRef = useRef<JSX.Element | FormField<any>>(null);
  const handleClickOut = () => {
    if (submitTrigger.includes('click')) {
      submit();
    }
  };
  const containerRef = useClickOut<HTMLDivElement>(handleClickOut);
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
      setEditing(false);
    }
  };
  const showEditor = () => {
    if (!editing) {
      setEditing(true);
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
  const handleEditorBlur = () => {
    if (submitTrigger.includes('blur')) {
      submit();
    }
  };
  const submit = async (newValue?: any) => {
    // 延缓submit，因为有时候blur之后才会onchange，保证拿到的值是最新的
    // setTimeout(async () => {

    // });
    let waitSubmitValue = (editorRef.current as any)?.getValue
      ? toJS((editorRef.current as any)?.getValue()) : dataRef.current;
    if (typeof (newValue) !== 'undefined') {
      waitSubmitValue = newValue;
    }

    console.log('submit patternMismatch', editingRef.current, waitSubmitValue, editorRef.current, await (editorRef.current as FormField<any>)?.validate(waitSubmitValue));
    // @ts-ignore
    if (editingRef.current && editorRef.current && await editorRef.current?.validate(waitSubmitValue)) {
      if (containerRef.current) {
        containerRef.current.blur();
      }
      hideEditor();
      if (waitSubmitValue !== initValue) {
        onSubmit(waitSubmitValue);
      }
    }
  };
  const renderEditor = () => {
    const editorElement = typeof editor === 'function' ? editor({ submit }) : editor;
    if (!editing && !alwaysRender) {
      return null;
    }
    const extraContent = typeof editorExtraContent === 'function' ? editorExtraContent() : editorExtraContent;
    const originProps = editorElement.props;
    const editorProps: any = {
      value,
      onChange: handleChange(originProps.onChange),
      onBlur: handleEditorBlur,
      ref: editorRef,
    };
    if (containerRef.current) {
      editorProps.style = {
        width: containerRef.current.getBoundingClientRect().width,
        height: containerRef.current.getBoundingClientRect().height,
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
