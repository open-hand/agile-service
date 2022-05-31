import React, {
  createRef,
  RefObject,
} from 'react';
import { pick } from 'lodash';
import { useCreation, useDebounceFn, usePersistFn } from 'ahooks';
import { render } from 'react-dom';
import useTooltip from './useTooltip';
import '../style/follow-mouse.less';

interface IHookFollowMouseTooltipProps {
  tooltipTitle?: React.ReactNode
}
let currentDom: any;
function getRoot() {
  const dom = document.body.querySelector('div.c7n-agile-follow-mouse-root');
  if (dom) {
    return dom;
  }
  const root = document.createElement('div');
  root.className = 'c7n-agile-follow-mouse-root';
  document.body.appendChild(root);
  return root;
}
async function getMouseContainer(): Promise<RefObject<HTMLDivElement>> {
  if (currentDom) {
    return currentDom;
  }
  return new Promise((resolve) => {
    const root = getRoot();
    if (root) {
      const ref = createRef<HTMLDivElement>();
      render(<div ref={ref} className="c7n-agile-follow-mouse-dom" />, root, () => {
        currentDom = ref;
        resolve(ref);
      });
    }
  });
}
/**
 * 跟随鼠标移动的tooltip
 * @param param0
 * @returns
 */
function useFollowMouseTooltip({ tooltipTitle }: IHookFollowMouseTooltipProps) {
  const { onMouseLeave, onMouseEnter, updateTooltipProps } = useTooltip({ tooltip: { title: tooltipTitle } });
  const { run: showTooltip, cancel } = useDebounceFn(async ({ pageX, pageY }: any) => {
    const targetRef = await getMouseContainer();
    if (targetRef.current) {
      targetRef.current.setAttribute('style', `left:${pageX}px;top:${pageY}px;`);
    }
    onMouseEnter({ target: targetRef.current }, { arrowPointAtCenter: true });
  }, { wait: 600 });

  const handleTooltipMouseEnter = usePersistFn((event: React.MouseEvent) => {
    event.preventDefault();
    event.stopPropagation();
    showTooltip(pick(event, ['pageX', 'pageY']));
  });

  const actions = useCreation(() => ({ update: updateTooltipProps }), []);
  return [{ actions }, {
    onMouseMove: handleTooltipMouseEnter,
    onMouseEnter: handleTooltipMouseEnter,
    onMouseLeave: (event: React.MouseEvent) => {
      event.preventDefault();
      event.stopPropagation();
      cancel();
      onMouseLeave();
    },
  }] as const;
}
export default useFollowMouseTooltip;
