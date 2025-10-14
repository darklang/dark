import { InstanceNode } from "../../types";

export class InstanceDemoData {
  static getInstancesData(): InstanceNode[] {
    return [
      // Local instance (current/selected)
      {
        id: "local-instance",
        label: "★ Local (/home/stachu/code/dark)",
        type: "current",
        contextValue: "current-instance",
        instanceData: {
          path: "/home/stachu/code/dark",
          status: "connected",
        },
        children: []
      },

      // matter.darklang.com
      {
        // uuid, not string
        id: "matter-instance",
        // 'name'
        label: "matter.darklang.com",
        type: "remote",
        contextValue: "remote-instance",
        instanceData: {
          url: "https://matter.darklang.com",
          status: "connected"
        },
        children: []
      }
    ];
  }
}