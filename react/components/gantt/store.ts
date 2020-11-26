/* eslint-disable no-shadow */
/* eslint-disable no-param-reassign */
/* eslint-disable no-underscore-dangle */
import { createRef } from 'react';
import {
  observable, computed, action, toJS,
} from 'mobx';
import { debounce, find } from 'lodash';
import dayjs, { Dayjs } from 'dayjs';
import weekOfYear from 'dayjs/plugin/weekOfYear';
import quarterOfYear from 'dayjs/plugin/quarterOfYear';
import isBetween from 'dayjs/plugin/isBetween';
import advancedFormat from 'dayjs/plugin/advancedFormat';
import isLeapYear from 'dayjs/plugin/isLeapYear';
import weekday from 'dayjs/plugin/weekday';
import { Gantt } from './types';
import {
  ROW_HEIGHT, HEADER_HEIGHT, CELL_UNIT, MOVE_SPACE, MIN_VIEW_RATE,
} from './constants';
import {
  flattenDeep, getDragSideShrink, getDragSideExpand, getMoveStep,
} from './utils';

dayjs.extend(weekday);
dayjs.extend(weekOfYear);
dayjs.extend(quarterOfYear);
dayjs.extend(advancedFormat);
dayjs.extend(isBetween);
dayjs.extend(isLeapYear);

// 视图日视图、周视图、月视图、季视图、年视图
export const viewTypeList: Gantt.SightConfig[] = [
  {
    type: 'day',
    label: '日',
    value: 2880,
  },
  {
    type: 'week',
    label: '周',
    value: 3600,
  },
  {
    type: 'month',
    label: '月',
    value: 14400,
  },
  {
    type: 'quarter',
    label: '季',
    value: 86400,
  },
  {
    type: 'halfYear',
    label: '年',
    value: 115200,
  },
];
const topTap = 4;
const startDate = '2020-10-01';
class GanttStore {
  constructor() {
    this.width = 1320;
    this.height = 418;
    const sightConfig = viewTypeList[0];
    const translateX = dayjs(startDate).valueOf() / (sightConfig.value * 1000);
    const bodyWidth = this.width;
    const viewWidth = 704;
    const tableWidth = 500;
    // const collapsed = this.data.every(bar => bar.collapsed);
    this.viewWidth = viewWidth;
    this.tableWidth = tableWidth;
    this.translateX = translateX;
    this.sightConfig = sightConfig;
    this.bodyWidth = bodyWidth;
  }

  _wheelTimer: NodeJS.Timeout | null

  @observable data: Gantt.Item[] = [];

  @observable columns: Gantt.Column[] = [];

  @observable scrolling = false;

  @observable collapse = false;

  @observable tableWidth: number;

  @observable viewWidth: number;

  @observable width: number;

  @observable height: number;

  @observable bodyWidth: number;

  @observable translateX: number;

  @observable sightConfig: Gantt.SightConfig;

  @observable showSelectionIndicator: boolean = false;

  @observable selectionIndicatorTop: number = 0;

  @observable dragging: Gantt.Bar | null = null;

  @observable draggingType: Gantt.MoveType | null = null

  gestureKeyPress: boolean = false;

  mainElementRef = createRef<HTMLDivElement>();

  chartElementRef = createRef<HTMLDivElement>();

  isPointerPress: boolean = false;

  @action
  setData(data: Gantt.Item[]) {
    this.data = data;
  }

  @action
  toggleCollapse() {
    if (this.tableWidth > 0) {
      this.tableWidth = 0;
      this.viewWidth = this.width - this.tableWidth;
    } else {
      this.initSize();
    }
  }

  @action
  setRowCollapse(item: Gantt.Item, collapsed: boolean) {
    item.collapsed = collapsed;
    // this.barList = this.getBarList();
  }

  @action
  setColumns(columns: Gantt.Column[]) {
    this.columns = columns;
  }

  @action syncSize(size: {
    width?: number;
    height?: number;
  }) {
    if (!size.height || !size.width) {
      return;
    }
    const { width, height } = size;
    this.width = width;
    this.height = height;
    // this.viewHeight = height - HEADER_HEIGHT;
    this.initSize();
  }

  @action initSize() {
    this.tableWidth = this.columns.reduce((width, item) => width + item.width, 0);
    this.viewWidth = this.height - this.tableWidth;
    // 表盘宽度不能小于总宽度38%
    if (this.viewWidth < MIN_VIEW_RATE * this.width) {
      this.viewWidth = MIN_VIEW_RATE * this.width;
      this.tableWidth = this.width - this.viewWidth;
    }

    // 图表宽度不能小于 200
    if (this.viewWidth < 200) {
      this.viewWidth = 200;
      this.tableWidth = this.width - this.viewWidth;
    }
  }

  @action switchSight(type: Gantt.Sight) {
    const target = find(viewTypeList, { type });
    if (target) {
      this.sightConfig = target;
      this.translateX = dayjs(startDate).valueOf() / (target.value * 1000);
    }
  }

  @action scrollToToday() {
    const translateX = this.todayTranslateX - (this.viewWidth / 2);
    this.translateX = translateX;
  }

  @computed get todayTranslateX() {
    return Math.floor(dayjs(new Date().valueOf()).hour(0).minute(0).second(0)
      .valueOf() / this.pxUnitAmp);
  }

  // 内容区滚动高度
  @computed get bodyClientHeight() {
    return this.height - HEADER_HEIGHT;
  }

  @computed get getColumnsWidth(): number[] {
    const totalColumnWidth = this.columns.reduce((width, item) => width + item.width, 0);
    if (totalColumnWidth < this.tableWidth) {
      let availableWidth = this.tableWidth;
      const result: number[] = [];
      this.columns.forEach((column, index) => {
        if (index === this.columns.length - 1) {
          result.push(availableWidth);
        } else {
          const width = (this.tableWidth * (column.width / totalColumnWidth));
          result.push(width);
          availableWidth -= width;
        }
      });
      return result;
    }
    return this.columns.map((column) => column.width);
  }

  // 内容区滚动区域域高度
  @computed get bodyScrollHeight() {
    let height = this.getBarList.length * ROW_HEIGHT + topTap;
    if (height < this.bodyClientHeight) {
      height = this.bodyClientHeight;
    }
    return height;
  }

  @computed get pxUnitAmp() {
    return this.sightConfig.value * 1000;
  }

  /**
   * 时间起始偏移量
   */
  @computed get translateAmp() {
    const { translateX } = this;
    return this.pxUnitAmp * translateX;
  }

  getDurationAmp() {
    const clientWidth = this.viewWidth;
    return this.pxUnitAmp * clientWidth;
  }

  getMajorList(): Gantt.Major[] {
    const majorFormatMap: { [key in Gantt.Sight]: string } = {
      day: 'YYYY年 MM月',
      week: 'YYYY年 MM月',
      month: 'YYYY年',
      quarter: 'YYYY年',
      halfYear: 'YYYY年',
    };
    const { translateAmp } = this;
    const endAmp = translateAmp + this.getDurationAmp();
    const { type } = this.sightConfig;
    const format = majorFormatMap[type];

    const getNextDate = (start: Dayjs) => {
      if (type === 'day' || type === 'week') {
        return start.add(1, 'month');
      }
      return start.add(1, 'year');
    };

    const getStart = (date: Dayjs) => {
      if (type === 'day' || type === 'week') {
        return date.startOf('month');
      }
      return date.startOf('year');
    };

    const getEnd = (date: Dayjs) => {
      if (type === 'day' || type === 'week') {
        return date.endOf('month');
      }
      return date.endOf('year');
    };

    // 初始化当前时间
    let curDate = dayjs(translateAmp);
    const dateMap = new Map<string, Gantt.MajorAmp>();

    // 对可视区域内的时间进行迭代
    while (curDate.isBetween(translateAmp - 1, endAmp + 1)) {
      const majorKey = curDate.format(format);

      let start = curDate;
      const end = getEnd(start);
      if (dateMap.size !== 0) {
        start = getStart(curDate);
      }

      if (!dateMap.has(majorKey)) {
        dateMap.set(majorKey, {
          label: majorKey,
          startDate: start,
          endDate: end,
        });
      }

      // 获取下次迭代的时间
      start = getStart(curDate);
      curDate = getNextDate(start);
    }

    return this.majorAmp2Px([...dateMap.values()]);
  }

  majorAmp2Px(ampList: Gantt.MajorAmp[]) {
    const { pxUnitAmp } = this;
    const list = ampList.map((item) => {
      const { startDate } = item;
      const { endDate } = item;
      const { label } = item;
      const left = (startDate.valueOf() / pxUnitAmp);
      const width = (endDate.valueOf() - startDate.valueOf()) / pxUnitAmp;

      return {
        label,
        left,
        width,
      };
    });
    return list;
  }

  getMinorList(): Gantt.Minor[] {
    const minorFormatMap = {
      day: 'YYYY-MM-D',
      week: 'YYYY-w周', // format W 不知道为什么不支持周，文档却说支持,
      month: 'YYYY-MM月',
      quarter: 'YYYY-第Q季',
      halfYear: 'YYYY-',
    };
    const fstHalfYear = [0, 1, 2, 3, 4, 5];

    const startAmp = this.translateAmp;
    const endAmp = startAmp + this.getDurationAmp();
    const format = minorFormatMap[this.sightConfig.type];

    const getNextDate = (start: Dayjs) => {
      const map = {
        day() {
          return start.add(1, 'day');
        },
        week() {
          return start.add(1, 'week');
        },
        month() {
          return start.add(1, 'month');
        },
        quarter() {
          return start.add(1, 'quarter');
        },
        halfYear() {
          return start.add(6, 'month');
        },
      };

      return (map[this.sightConfig.type])();
    };
    const setStart = (date: Dayjs) => {
      const map = {
        day() {
          return date.startOf('day');
        },
        week() {
          return date.weekday(1).hour(0).minute(0).second(0);
        },
        month() {
          return date.startOf('month');
        },
        quarter() {
          return date.startOf('quarter');
        },
        halfYear() {
          if (fstHalfYear.includes(date.month())) {
            return date.month(0).startOf('month');
          }
          return date.month(6).startOf('month');
        },
      };

      return (map[this.sightConfig.type])();
    };
    const setEnd = (start: Dayjs) => {
      const map = {
        day() {
          return start.endOf('day');
        },
        week() {
          return start.weekday(7).hour(23).minute(59).second(59);
        },
        month() {
          return start.endOf('month');
        },
        quarter() {
          return start.endOf('quarter');
        },
        halfYear() {
          if (fstHalfYear.includes(start.month())) {
            return start.month(5).endOf('month');
          }
          return start.month(11).endOf('month');
        },
      };

      return (map[this.sightConfig.type])();
    };
    const getMinorKey = (date: Dayjs) => {
      if (this.sightConfig.type === 'halfYear') {
        return date.format(format) + (fstHalfYear.includes(date.month()) ? '上半年' : '下半年');
      }

      return date.format(format);
    };

    // 初始化当前时间
    let curDate = dayjs(startAmp);
    const dateMap = new Map<string, Gantt.MinorAmp>();

    while (curDate.isBetween(startAmp - 1, endAmp + 1)) {
      const minorKey = getMinorKey(curDate);

      const start = setStart(curDate);
      const end = setEnd(start);
      if (!dateMap.has(minorKey)) {
        dateMap.set(minorKey, {
          label: minorKey.split('-').pop() as string,
          startDate: start,
          endDate: end,
        });
      }

      curDate = getNextDate(start);
    }

    return this.minorAmp2Px([...dateMap.values()]);
  }

  startXRectBar = (startX: number) => {
    let date = dayjs(startX * this.pxUnitAmp);
    const dayRect = () => {
      const stAmp = date.startOf('day');
      const endAmp = date.endOf('day');
      // @ts-ignore
      const left = stAmp / this.pxUnitAmp;
      // @ts-ignore
      const width = (endAmp - stAmp) / this.pxUnitAmp;

      return {
        left,
        width,
      };
    };
    const weekRect = () => {
      // week 注意周日为每周第一天 ????????
      if (date.weekday() === 0) {
        date = date.add(-1, 'week');
      }
      const left = date.weekday(1).startOf('day').valueOf() / this.pxUnitAmp;
      const width = (7 * 24 * 60 * 60 * 1000 - 1000) / this.pxUnitAmp;

      return {
        left,
        width,
      };
    };
    const monthRect = () => {
      const stAmp = date.startOf('month').valueOf();
      const endAmp = date.endOf('month').valueOf();
      const left = stAmp / this.pxUnitAmp;
      const width = (endAmp - stAmp) / this.pxUnitAmp;

      return {
        left,
        width,
      };
    };

    const map = {
      day: dayRect,
      week: weekRect,
      month: weekRect,
      quarter: monthRect,
      halfYear: monthRect,
    };

    return map[this.sightConfig.type]();
  }

  @action
  handleInvalidBarLeave() {
    this.handleDragEnd();
  }

  @action
  handleInvalidBarHover(barInfo: Gantt.Bar, left: number, width: number) {
    barInfo.translateX = left;
    barInfo.width = width;
    // 只能向右拖动
    this.handleDragStart(barInfo, 'right');
  }

  @action
  handleInvalidBarMove(barInfo: Gantt.Bar, left: number, width: number) {
    const moveDistance = left - barInfo.translateX;
    // 只能向右拖动
    if (moveDistance + width > 0) {
      barInfo.width = moveDistance + width;
    }
  }

  @action
  handleInvalidBarDown(barInfo: Gantt.Bar) {
    // 只能向右拖动
    barInfo.stepGesture = 'moving';
  }

  @action
  handleInvalidBarUp(barInfo: Gantt.Bar) {
    barInfo.invalidDateRange = false;
    this.handleDragEnd();
    // TODO 修改日期逻辑
  }

  minorAmp2Px(ampList: Gantt.MinorAmp[]): Gantt.Minor[] {
    const { pxUnitAmp } = this;
    const list = ampList.map((item) => {
      const startDate = item.startDate.hour(0).minute(0).second(0);
      const endDate = item.endDate.hour(23).minute(59).second(59);

      const { label } = item;
      const left = Math.ceil(startDate.valueOf() / pxUnitAmp);
      const width = Math.ceil((endDate.valueOf() - startDate.valueOf()) / pxUnitAmp);

      let isWeek = false;
      if (this.sightConfig.type === 'day') {
        isWeek = [0, 6].includes(startDate.weekday());
      }
      return {
        label,
        left,
        width,
        isWeek,
        key: startDate.format('YYYY-MM-DD HH:mm:ss'),
      };
    });
    return list;
  }

  @computed get getBarList(): Gantt.Bar[] {
    const { pxUnitAmp, data } = this;
    const minStamp = 11 * pxUnitAmp;
    const height = 8;
    const baseTop = 14;
    const topStep = 28;

    // TODO 后期需优化 增加上周下周等内容
    const dateTextFormat = (startX: number) => dayjs(startX * pxUnitAmp).format('YYYY-MM-DD');
    const _dateFormat = (date: string) => {
      if (!date) return '待设置';
      return dayjs(date).format('YYYY年MM月DD日');
    };

    // 获取鼠标位置所在bar大小及位置
    const startXRectBar = (startX: number) => {
      let date = dayjs(startX * pxUnitAmp);
      const dayRect = () => {
        const stAmp = date.startOf('day');
        const endAmp = date.endOf('day');
        // @ts-ignore
        const left = stAmp / pxUnitAmp;
        // @ts-ignore
        const width = (endAmp - stAmp) / pxUnitAmp;

        return {
          left,
          width,
        };
      };
      const weekRect = () => {
        // week 注意周日为每周第一天 ????????
        if (date.weekday() === 0) {
          date = date.add(-1, 'week');
        }

        const left = date.weekday(1).startOf('day').valueOf() / pxUnitAmp;
        const width = (7 * 24 * 60 * 60 * 1000 - 1000) / pxUnitAmp;

        return {
          left,
          width,
        };
      };
      const monthRect = () => {
        const stAmp = date.startOf('month').valueOf();
        const endAmp = date.endOf('month').valueOf();
        const left = stAmp / pxUnitAmp;
        const width = (endAmp - stAmp) / pxUnitAmp;

        return {
          left,
          width,
        };
      };

      const map = {
        day: dayRect,
        week: weekRect,
        month: weekRect,
        quarter: monthRect,
        halfYear: monthRect,
      };

      return map[this.sightConfig.type]();
    };

    // // 设置阴影位置
    // const setShadowShow = (left: number, width: number, isShow: boolean) => {
    //   this.dragPresentVisible = isShow;
    //   this.shadowGestBarLeft = left;
    //   this.shadowGestBarRight = left + width;
    //   this.dragPresentX = left;
    //   this.dragPresentWidth = width;
    // };

    // // 设置任务
    // const setInvalidTaskBar = (barInfo: Gantt.Bar, left: number, width: number) => {
    //   barInfo.translateX = left;
    //   barInfo.width = width;
    //   barInfo.invalidDateRange = false;

    //   this.dragPresentVisible = true;
    //   this.shadowGestBarLeft = left + width;
    //   this.shadowGestBarRight = 0;

    //   this.dragPresentX = left;
    //   this.dragPresentWidth = width;

    //   barInfo.stepGesture = 'moving';
    // };
    /**
     * 根据选中行高度 显示对应条状工具条
     */
    // const getHovered = (top: number, selectionIndicatorTop: number) => {
    //   const baseTop = top - (top % ROW_HEIGHT);
    //   const isShow = (selectionIndicatorTop >= baseTop && selectionIndicatorTop <= baseTop + ROW_HEIGHT);

    //   return isShow;
    // };

    // 进行展开扁平
    return observable(flattenDeep(data).map((item: any, index) => {
      let startAmp = dayjs(item.startDate || 0).valueOf();
      let endAmp = dayjs(item.endDate || 0).valueOf();

      // 开始结束日期相同默认一天
      if (Math.abs(endAmp - startAmp) < minStamp) {
        startAmp = dayjs(item.startDate || 0).valueOf();
        endAmp = dayjs(item.endDate || 0).add(minStamp, 'millisecond').valueOf();
      }

      const width = (endAmp - startAmp) / pxUnitAmp;
      const translateX = startAmp / pxUnitAmp;
      const translateY = baseTop + index * topStep;
      const { _parent } = item;

      return {
        task: item,
        translateX,
        translateY,
        width,
        height,
        label: item.content,
        stepGesture: 'end', // start(开始）、moving(移动)、end(结束)
        invalidDateRange: !item.endDate || !item.startDate, // 是否为有效时间区间
        dateTextFormat, // TODO 日期格式化函数 后期根据当前时间格式化为上周下周,
        startXRectBar, // 鼠标位置 获取创建bar位置及大小
        // setShadowShow,
        // setInvalidTaskBar,
        // getHovered,
        _collapsed: item.collapsed, // 是否折叠
        _depth: item._depth, // 表示子节点深度
        _index: item._index, // 任务下标位置
        _parent, // 原任务数据
        _childrenCount: !item.children ? 0 : item.children.length, // 子任务
        _dateFormat,
      };
    }));
  }

  @action
  handleWheel = (event: React.WheelEvent<HTMLDivElement>) => {
    if (this._wheelTimer) clearTimeout(this._wheelTimer);
    // 水平滚动
    if (Math.abs(event.deltaX) > 0) {
      this.scrolling = true;
      this.translateX += event.deltaX;
    }
    this._wheelTimer = setTimeout(() => {
      this.scrolling = false;
    }, 100);
  }

  handleMouseMove = debounce((event) => {
    if (!this.isPointerPress) {
      this.showSelectionBar(event);
    }
  }, 5)

  handleMouseLeave() {
    this.showSelectionIndicator = false;
  }

  @action
  showSelectionBar(event: MouseEvent) {
    const scrollTop = this.mainElementRef.current?.scrollTop || 0;
    const { top } = this.mainElementRef.current?.getBoundingClientRect() || { top: 0 };
    // 内容区高度
    const contentHeight = this.getBarList.length * ROW_HEIGHT;
    const offsetY = event.clientY - top + scrollTop;
    if (offsetY - contentHeight > topTap) {
      this.showSelectionIndicator = false;
    } else {
      const top = Math.floor((offsetY - topTap) / ROW_HEIGHT) * ROW_HEIGHT + 4;
      this.showSelectionIndicator = true;
      this.selectionIndicatorTop = top;
    }
  }

  getHovered = (top: number) => {
    const baseTop = top - (top % ROW_HEIGHT);
    const isShow = (this.selectionIndicatorTop >= baseTop && this.selectionIndicatorTop <= baseTop + ROW_HEIGHT);
    return isShow;
  }

  @action
  handleDragStart(barInfo: Gantt.Bar, type: Gantt.MoveType) {
    this.dragging = barInfo;
    this.gestureKeyPress = true;
    this.draggingType = type;
    barInfo.stepGesture = 'start';
  }

  @action
  handleDragEnd() {
    if (this.dragging) {
      this.dragging.stepGesture = 'end';
      this.dragging = null;
    }
    this.gestureKeyPress = false;
    this.draggingType = null;
  }

  /**
   * 调整宽度
   * @param event
   * @param type
   * @param barInfo
   */
  @action
  shadowGesturePress(event: HammerInput, type: Gantt.MoveType, barInfo: Gantt.Bar) {
    if (!this.chartElementRef.current) {
      return;
    }
    const { width } = barInfo;
    this.isPointerPress = true;
    const isLeft = type === 'left';
    const { left, right } = event.target.getBoundingClientRect();
    const startX = isLeft ? right : left;
    // 移动右边，以左侧为基准
    const basePointerX = isLeft ? startX + width : startX - width;

    const chartHammer = new Hammer(this.chartElementRef.current);
    // let baseX: number;
    // const old = { ...barInfo };
    const panStart = (event: HammerInput) => {
      // baseX = event.center.x;
      this.handleDragStart(barInfo, type);
    };
    const panMove = (event: HammerInput) => {
      // 移动
      // eslint-disable-next-line no-bitwise
      // const moveDistance = (((event.center.x - baseX + 15) / 30) | 0) * 30;
      // 向左移动，值为负，向右移动，值为正
      // if (type === 'left') {
      //   // 移动左操作栏时
      //   barInfo.width = old.width - moveDistance;
      //   barInfo.translateX = old.translateX + moveDistance;
      // }
      this.updateDraggingBarPosition(event, barInfo, type, basePointerX);
    };
    const panEnd = () => {
      this.isPointerPress = false;
      this.handleDragEnd();
      chartHammer.destroy();
      this.updateTaskDate(barInfo);
    };

    chartHammer.on('panstart', panStart);
    chartHammer.on('panmove', panMove);
    chartHammer.on('panend', panEnd);
  }

  shadowGesturePressUp() {

  }

  /**
   * 横向调整位置
   * @param barInfo
   */
  shadowGestureBarPress(barInfo: Gantt.Bar) {
    if (!this.chartElementRef.current) {
      return;
    }
    const chartHammer = new Hammer(this.chartElementRef.current);
    const step = CELL_UNIT;
    let { translateX } = barInfo;

    let startX = 0;
    let pointerX = 0;

    const layoutShadow = action((translateX: number) => {
      barInfo.translateX = translateX;
    });

    const setBarShadowPosition = action((event: HammerInput) => {
      pointerX = event.center.x;
      const pointerDis = pointerX - startX;
      const direction = pointerDis > 0 ? 1 : -1;
      const moveX = step * direction;
      if (Math.abs(pointerDis) >= step) {
        translateX += moveX;
        layoutShadow(translateX);
        startX += moveX;
        barInfo.stepGesture = 'moving';
      }
    });
    const panStart = (event: HammerInput) => {
      startX = event.center.x;
      this.handleDragStart(barInfo, 'move');
    };

    const panMove = (event: HammerInput) => {
      setBarShadowPosition(event);
    };

    const panEnd = () => {
      this.handleDragEnd();
      chartHammer.destroy();
      this.updateTaskDate(barInfo);
    };

    chartHammer.on('panstart', panStart);
    chartHammer.on('panmove', panMove);
    chartHammer.on('panend', panEnd);
  }

  @action
  shadowGestureBarPressUp() {

  }

  /**
     * 更新时间
     */
  @action
  updateTaskDate(barInfo: Gantt.Bar) {
    const { translateX } = barInfo;
    const { width } = barInfo;
    const { task } = barInfo;

    task.startDate = String(dayjs(translateX * this.pxUnitAmp));
    task.endDate = String(dayjs((translateX + width) * this.pxUnitAmp));
    // TODO:更新之后的后续处理
  }

  @action
  updateDraggingBarPosition(moveEv: HammerInput, barInfo: Gantt.Bar, type: Gantt.MoveType, basePointerX: number) {
    const isLeft = type === 'left';
    const pointerX = moveEv.center.x;
    const isShrink = getDragSideShrink(moveEv, type);
    const isExpand = getDragSideExpand(moveEv, type);
    // 每次step可能不一样， 动态计算 如：每月可能30或31天
    const step = getMoveStep(isLeft, isShrink, this.sightConfig.type, this.pxUnitAmp, barInfo);

    if (isShrink) {
      this.moveShrinkStep(step, type, barInfo, basePointerX, pointerX);
    }

    if (isExpand) {
      this.moveExpandStep(step, type, barInfo, basePointerX, pointerX);
    }
  }

  /**
     * 跟随鼠标移动搜索阴影
     */
  @action
  moveShrinkStep = (step: number, type: Gantt.MoveType, barInfo: Gantt.Bar, basePointerX: number, pointerX: number) => {
    const isLeft = type === 'left';
    let { width, translateX } = barInfo;

    if (isLeft) {
      translateX += step;
      width -= step;
    } else {
      width -= step;
    }

    const pointerDis = Math.abs(pointerX - basePointerX);
    if (pointerDis > width) return;
    if (width <= step) return;

    barInfo.translateX = translateX;
    barInfo.width = width;
  }

  /**
   * 跟随鼠标拖动扩大阴影
   */
  @action
  moveExpandStep = (step: number, type: Gantt.MoveType, barInfo: Gantt.Bar, basePointerX: number, pointerX: number) => {
    const isLeft = type === 'left';
    let { width, translateX } = barInfo;

    const pointerDis = Math.abs(pointerX - basePointerX);
    if (pointerDis < MOVE_SPACE || pointerDis < width) return;

    // 测试代码
    if (isLeft) {
      translateX -= step;
      width += step;
    } else {
      width += step;
    }
    barInfo.translateX = translateX;
    barInfo.width = width;
  }
}

export default GanttStore;
