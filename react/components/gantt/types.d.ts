import { Dayjs } from 'dayjs';

export namespace Gantt {
  interface Major {
    width: number
    left: number
    label: string
  }
  interface MajorAmp {
    label: string,
    startDate: Dayjs,
    endDate: Dayjs,
  }
  interface Minor {
    width: number
    left: number
    label: string
    isWeek: boolean
    key: string
  }
  interface MinorAmp {
    label: string,
    startDate: Dayjs,
    endDate: Dayjs,
  }
  type Sight = 'day' | 'week' | 'month' | 'quarter' | 'halfYear'
  type MoveType = 'left' | 'right' | 'move'
  interface SightConfig {
    type: Sight,
    label: string
    value: number
  }
  interface Bar {
    label: string
    width: number
    translateX: number
    translateY: number
    stepGesture: string
    invalidDateRange: boolean
    dateTextFormat: (startX: number) => string
    task: Item
    _collapsed: boolean
    _depth: number
    _index?: number
    _childrenCount: number
    _parent?: Bar
  }
  interface Item {
    startDate: string | null,
    endDate: string | null,
    collapsed: boolean,
    children?: Item[],
    borderColor?: string
    backgroundColor?: string
    _parent?: Bar
    _depth?: number
    _index?: number
  }
  interface Column {
    width: number,
    name: string,
    label: string,
    render?: (item: Item) => React.ReactNode
  }
}
